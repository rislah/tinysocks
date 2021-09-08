defmodule Tinysocks.Protocol.Message do
  defstruct binary: <<>>, state: "", misc: []

  def new(state, bin, misc \\ []) do
    %__MODULE__{binary: bin, state: state, misc: misc}
  end
end
