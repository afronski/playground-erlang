defmodule Agenda do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(Agenda.Worker, [:ok])
    ]

    opts = [strategy: :one_for_one, name: Agenda.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defdelegate add_schedule(schedule), to: Agenda.Worker
  defdelegate clear_schedule(), to: Agenda.Worker
end
