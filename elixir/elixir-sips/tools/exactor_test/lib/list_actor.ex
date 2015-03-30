defmodule ListActor do
  use ExActor.GenServer

  defstart start(x), do: initial_state(x)

  defcall get, state: state, do: reply(state)
  defcast put(x), state: state, do: new_state(state ++ [x])
  defcast take(x), state: state, do: new_state(List.delete(state, x))
end
