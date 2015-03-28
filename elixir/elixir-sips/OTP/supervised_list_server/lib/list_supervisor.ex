defmodule ListSupervisor do
  use Supervisor

  ### Public API
  def start_link do
    result = {:ok, supervisor} = :supervisor.start_link(__MODULE__, [])
    start_workers(supervisor)
    result
  end

  def start_workers(supervisor) do
    {:ok, list_data} = :supervisor.start_child(supervisor, worker(ListData, []))
    :supervisor.start_child(supervisor, worker(ListSubSupervisor, [list_data]))
  end

  ### Supervisor API
  def init(_) do
    supervise [], strategy: :one_for_one
  end
end
