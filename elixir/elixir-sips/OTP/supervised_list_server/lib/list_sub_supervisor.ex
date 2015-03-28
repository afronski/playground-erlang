defmodule ListSubSupervisor do
  use Supervisor

  ### Public API
  def start_link(list_data_pid) do
    :supervisor.start_link(__MODULE__, list_data_pid)
  end

  ### Supervisor API
  def init(list_data_pid) do
    child_processes = [ worker(ListServer, [list_data_pid]) ]
    supervise child_processes, strategy: :one_for_one
  end
end
