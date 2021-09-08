defmodule Tinysocks.Protocol.Parser do
  @supported_version 5
  @no_auth 0
  @user_pass 2
  alias Tinysocks.Protocol.Message, as: Message

  def parse_packet(<<_version, num_methods, methods::binary-size(num_methods)>>, _) do
    methods_list = :binary.bin_to_list(methods)
    method = choose_auth_method(methods_list)
    Message.new(Atom.to_string(:greeting), <<@supported_version, method>>)
  end

  defp choose_auth_method([]) do
    0xFF
  end

  defp choose_auth_method([head | tail]) do
    choose_auth_method(head, tail)
  end

  defp choose_auth_method(@no_auth, _tail) do
    @no_auth
  end

  defp choose_auth_method(@user_pass, _tail) do
    @user_pass
  end

  defp choose_auth_method(_head, tail) do
    choose_auth_method(tail)
  end
end
