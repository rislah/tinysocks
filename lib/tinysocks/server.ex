defmodule Tinysocks.Server do
  require Logger

  # @host Application.get_env(:socks, :host)
  @port Application.get_env(:tinysocks, :port)

  def start_link() do
    :ranch.start_listener(:Tinysocks, :ranch_tcp, [{:port, @port}], Tinysocks.Worker, [])
  end
end
