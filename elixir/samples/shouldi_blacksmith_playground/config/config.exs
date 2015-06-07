use Mix.Config

config :shouldi_blacksmith_playground, Repo,
  adapter: Ecto.Adapters.Postgres,
  database: "test",
  username: "postgres",
  password: "postgres"
