defmodule AbacusApplication do
  use Application

  def start(_type, args) do
    result = AbacusSupervisor.start_link(args)

    {:ok, pid} = result
    Process.register(pid, :abacus)

    result
  end
end
