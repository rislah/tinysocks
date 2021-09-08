defmodule TinysocksTest do
  use ExUnit.Case
  doctest Tinysocks

  test "greets the world" do
    assert Tinysocks.hello() == :world
  end
end
