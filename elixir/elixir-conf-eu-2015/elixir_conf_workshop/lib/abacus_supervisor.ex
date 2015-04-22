defmodule AbacusSupervisor do
  use Supervisor

  # Public API.

  def start_link(args) do
    {:ok, supervisor} = Supervisor.start_link(__MODULE__, [])

    Supervisor.start_child(supervisor, worker(AbacusServer, [ args ]))
  end

  # Supervisor API.

  def init(_args) do
    import Supervisor.Spec

    supervise([], strategy: :one_for_one)
  end
end
