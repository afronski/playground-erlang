use Mix.Config

config :json_api, JsonApi.Endpoint,
  http: [port: 4000],
  debug_errors: true,
  cache_static_lookup: false,
  watchers: []

config :json_api, JsonApi.Endpoint,
  live_reload: [Path.expand("priv/static/js/app.js"),
                Path.expand("priv/static/css/app.css"),
                Path.expand("web/templates/**/*.eex")]

config :phoenix, :code_reloader, true

config :logger, :console, format: "[$level] $message\n"

config :json_api, JsonApi.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "json_api_dev"
