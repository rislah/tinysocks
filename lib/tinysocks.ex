defmodule Tinysocks.Application do
  use Application
  use Supervisor
  require Logger

  def start(_start_type, _start_args) do
    children = [
      :ranch.child_spec(
        :network,
        :ranch_tcp,
        [{:port, 1088}],
        Tinysocks.Worker,
        []
      )
    ]
    Logger.info("socks5 server started")
    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
