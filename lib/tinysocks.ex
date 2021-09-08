defmodule Tinysocks.Application do
  use Application
  require Logger

  def start(_start_type, _start_args) do
    Logger.info("mhm")
    Tinysocks.Server.start_link()
  end
end
