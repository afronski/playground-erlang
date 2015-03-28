defmodule ListSupervisor do
  use Supervisor

  ### Public API
  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  ### Supervisor API
  def init(list) do
    child_processes = [ worker(ListServer, list) ]
    supervise child_processes, strategy: :one_for_one
  end
end
