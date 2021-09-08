defmodule Tinysocks.Message do
  defstruct stage: "", bin: <<>>, opts: []

  def new(stage, bin, opts \\ []) do
    %__MODULE__{stage: stage, bin: bin, opts: opts}
  end
end

defmodule Tinysocks.Worker do
  use GenServer
  require Logger
  alias Tinysocks.Message

  @supported_version 5
  @no_auth 0
  @behaviour :ranch_protocol
  @user_pass 2

  @spec start_link(any, any, any) :: {:ok, pid}
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
      opts: []
    })
  end

  def handle_info({:tcp, _, payload}, %{socket: socket, transport: transport, opts: opts} = state) do
    IO.inspect(payload)

    case parse_packet(payload, opts) do
      {:error, reason} ->
        Logger.error(fn ->
          "Parse packet error: #{inspect(reason)}"
        end)

        transport.close(socket)
        {:noreply, state}

      %Message{bin: bin, opts: opts} ->
        transport.send(socket, bin)
        transport.setopts(socket, active: :once)
        {:noreply, %{state | opts: opts}}
    end
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

  def parse_packet(
        <<version, username_len, username::binary-size(username_len), password_len,
          password::binary-size(password_len)>>,
        _
      ) do
    if username == "kasutaja" and password == "parool" do
      Message.new(Atom.to_string(:auth), <<version, 0>>)
    else
      Message.new(Atom.to_string(:auth), <<version, 1>>)
    end
  end

  def parse_packet(<<_version, num_methods, methods::binary-size(num_methods)>>, _) do
    methods_list = :binary.bin_to_list(methods)
    method = choose_method(methods_list)
    Message.new(Atom.to_string(:greeting), <<@supported_version, method>>)
  end

  def parse_packet(<<x::binary>>, opts) when length(opts) > 0 do
    [ip, port] = opts
    {:ok, sock} = :ranch_tcp.connect(List.to_tuple(ip), port, active: false)
    :ranch_tcp.send(sock, x)
    {:ok, packet} = :ranch_tcp.recv(sock, 0, 1000)
    :ranch_tcp.close(sock)
    IO.inspect(packet)
    Message.new(Atom.to_string(:request), packet , [])
  end


  def parse_packet(
        <<_version, _command, _rsv, 3, host_len, host::binary-size(host_len), port_significant, port>>,
        opts
      )
      when length(opts) == 0 do
    case :inet.getaddr(to_charlist(host), :inet) do
      {:ok, ip} ->
        Message.new(
          Atom.to_string(:request),
          <<@supported_version, 0, 0, 1>> <>
            :binary.list_to_bin(Tuple.to_list(ip)) <> <<port_significant, port>>,
          [Tuple.to_list(ip), :binary.decode_unsigned(<<port_significant, port>>)]
        )

      {:error, reason} ->
        {:error, reason}
    end
  end

  def parse_packet(<<5, _command, _rsv, 1, ip::binary-size(4), port_significant, port>>, opts)
      when length(opts) == 0 do
    Message.new(
      Atom.to_string(:request),
      <<5, 0, 0, 1>> <> ip <> <<port_significant, port>>,
      [:binary.bin_to_list(ip), :binary.decode_unsigned(<<port_significant, port>>)]
    )
  end

  defp choose_method([]) do
    0xFF
  end

  defp choose_method([head | tail]) do
    choose_method(head, tail)
  end

  defp choose_method(@no_auth, _tail) do
    @no_auth
  end

  defp choose_method(@user_pass, _tail) do
    @user_pass
  end

  defp choose_method(_head, tail) do
    choose_method(tail)
  end

  # defp read_loop(sock) do
  #   read_loop(sock, <<>>, 0)
  # end


  # defp read_loop(_sock, data, read) when read == 0 do
  #   data
  # end

  # defp read_loop(sock, data, _read) do
  #   {:ok, packet} = :ranch_tcp.recv(sock, 0, 1000)
  #   IO.inspect(packet)
  #   read_loop(sock, data <> packet, length(packet))
  # end
end
