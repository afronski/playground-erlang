use Mix.Config

config :json_api, JsonApi.Endpoint,
  http: [port: 4001],
  server: false

config :logger, level: :warn

config :json_api, JsonApi.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "json_api_test",
  size: 1,
  max_overflow: false
