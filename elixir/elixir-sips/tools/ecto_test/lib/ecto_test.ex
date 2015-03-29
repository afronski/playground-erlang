defmodule EctoTest do
  require Logger
  use Application

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      supervisor(EctoTest.Supervisor, [])
    ]

    Logger.debug "Application started."

    Supervisor.start_link children, strategy: :one_for_one
  end
end
