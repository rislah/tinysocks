defmodule Tinysocks.Server do
  require Logger

  @config %{host: "127.0.0.1", port: 1088}

  def start_link() do
    :ranch.start_listener(:Tinysocks, :ranch_tcp, [{:port, @config.port}], Tinysocks.Handler, [])
  end
end
