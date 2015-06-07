use Mix.Config

config :json_api, JsonApi.Endpoint,
  http: [port: {:system, "PORT"}],
  url: [host: "example.com"]

config :logger, level: :info

import_config "prod.secret.exs"
