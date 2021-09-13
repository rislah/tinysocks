defmodule Tinysocks.Worker do
  use GenServer
  require Logger

  @behaviour :ranch_protocol
  @no_auth 0
  @supported_version 5
  @user_pass 2
  @auth Application.get_env(:tinysocks, :auth)
  @accepted_creds Application.get_env(:tinysocks, :accepted_credentials)

  def start_link(ref, transport, opts) do
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, transport, opts])
    {:ok, pid}
  end

  def init(init_arg) do
    {:ok, init_arg}
  end

  def init(ref, transport, _opts) do
    {:ok, socket} = :ranch.handshake(ref)
    :ok = transport.setopts(socket, active: :once)

    :gen_server.enter_loop(__MODULE__, [], %{
      socket: socket,
      transport: transport,
      status: "",
      request_ip: 0,
      request_port: 0
    })
  end

  def handle_info({:tcp, _, payload}, state) do
    reply =
      case check_packet_type(payload) do
        {:greeting, [_num_methods | methods]} ->
          method = choose_method(:binary.bin_to_list(List.first(methods)))
          Logger.debug(":greeting | auth: #{inspect(@auth)}, method: #{inspect(method)}")

          if @auth and method != @user_pass do
            state.transport.send(state.socket, <<5, 0xFF>>)
          else
            state.transport.send(state.socket, <<5, method>>)
          end

          state.transport.setopts(state.socket, active: :once)
          {:noreply, %{state | status: "greeting"}}

        {:auth, [username | password]} ->
          bin =
            if username == "kasutaja" and List.first(password) == "parool" do
              <<5, 0>>
            else
              <<5, 1>>
            end

          state.transport.setopts(state.socket, active: :once)
          state.transport.send(state.socket, bin)
          {:noreply, %{state | status: "auth"}}

        {:request, [ip | port]} ->
          ip_bin = :binary.list_to_bin(Tuple.to_list(ip))
          port = List.first(port)

          bin = <<5, 0, 0, 1>> <> ip_bin <> port_to_bin(port)
          state.transport.send(state.socket, bin)
          state.transport.setopts(state.socket, active: false)

          case :gen_tcp.connect(ip, port, active: false) do
            {:ok, target} ->
              forwarding(target, state.socket)

            {:error, reason} ->
              IO.inspect(reason)
          end

          {:noreply, %{state | status: "proxying"}}

        {:error, reason} ->
          case reason do
            :nxdomain ->
              state.transport.send(state.socket, <<5, 4, 0::size(64)>>)
          end

          :ranch_tcp.close(state.socket)
          {:stop, :shutdown, state}
      end

    reply
  end

  def handle_info({:tcp_closed, _}, state) do
    Logger.info(fn ->
      "Client disconnected"
    end)

    {:stop, :normal, state}
  end

  def handle_info({:tcp_error, _, reason}, state) do
    Logger.info(fn ->
      "TCP error: #{inspect(reason)}"
    end)

    {:stop, :normal, state}
  end

  def port_to_bin(port) do
    case :binary.encode_unsigned(port) do
      <<significant, port>> ->
        <<significant, port>>

      <<port>> ->
        <<0, port>>
    end
  end

  def check_packet_type(<<_version, _command, _rsv, 3, host_len, host::binary-size(host_len), port_significant, port>>) do
    case :inet.getaddr(to_charlist(host), :inet) do
      {:ok, ip} ->
        {:request, [ip, :binary.decode_unsigned(<<port_significant, port>>)]}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def check_packet_type(<<_version, _command, _rsv, 1, ip::binary-size(4), port_significant, port>>) do
    {:request, [List.to_tuple(:binary.bin_to_list(ip)), :binary.decode_unsigned(<<port_significant, port>>)]}
  end

  def check_packet_type(
        <<_version, username_len, username::binary-size(username_len), password_len,
          password::binary-size(password_len)>>
      ) do
    {:auth, [username, password]}
  end

  def check_packet_type(<<_version, num_methods, methods::binary-size(num_methods)>>) do
    {:greeting, [num_methods, methods]}
  end

  defp forwarding(target, client) do
    {:ok, afterward_pid} = Task.Supervisor.start_child(Tinysocks.TaskSupervisor, fn -> forward(client, target) end)
    :gen_tcp.controlling_process(client, afterward_pid)

    {:ok, backward_pid} = Task.Supervisor.start_child(Tinysocks.TaskSupervisor, fn -> forward(target, client) end)
    :gen_tcp.controlling_process(target, backward_pid)
  end

  defp forward(from, to) do
    with {:ok, data} <- :gen_tcp.recv(from, 0),
         :ok <- :gen_tcp.send(to, data) do
      forward(from, to)
    else
      _ ->
        :gen_tcp.close(from)
        :gen_tcp.close(to)
    end
  end

  defp choose_method([]) do
    0xFF
  end

  defp choose_method([head | tail]) do
    choose_method(head, tail)
  end

  defp choose_method(@user_pass, _tail) when @auth do

    @user_pass
  end


  defp choose_method(@no_auth, _tail) when not @auth do
    @no_auth
  end

  defp choose_method(_head, tail) do
    choose_method(tail)
  end
end
