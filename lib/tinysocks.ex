defmodule Tinysocks.Application do
  use Application
  use Supervisor
  require Logger

  @port Application.get_env(:tinysocks, :port)

  def start(_start_type, _start_args) do
    children = [
      :ranch.child_spec(
        :network,
        :ranch_tcp,
        [{:port, @port}],
        Tinysocks.Worker,
        []
      ),
      {Task.Supervisor, name: Tinysocks.TaskSupervisor}
    ]
    Logger.info("socks5 server started")
    Supervisor.start_link(children, strategy: :one_for_one, name: __MODULE__)
  end
end
