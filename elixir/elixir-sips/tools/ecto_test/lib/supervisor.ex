defmodule EctoTest.Supervisor do
  use Supervisor

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    import Supervisor.Spec

    children = [
      worker(EctoTest.Repo, [])
    ]

    supervise children, strategy: :one_for_one
  end
end
