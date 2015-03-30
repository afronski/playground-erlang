defmodule CountActor do
  use ExActor.GenServer, export: :counter

  defstart start(x), do: initial_state(x)

  defcall get, state: state, do: reply(state)
  defcast inc, state: state, do: new_state(state + 1)
  defcast dec, state: state, do: new_state(state - 1)
end
