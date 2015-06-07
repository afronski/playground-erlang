defmodule JsonApi do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(JsonApi.Endpoint, []),
      worker(JsonApi.Repo, []),
    ]

    opts = [strategy: :one_for_one, name: JsonApi.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def config_change(changed, _new, removed) do
    JsonApi.Endpoint.config_change(changed, removed)
    :ok
  end
end
