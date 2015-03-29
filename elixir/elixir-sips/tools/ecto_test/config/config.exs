use Mix.Config

config :ecto_test, EctoTest.Repo,
  adapter: Ecto.Adapters.Postgres,
  database: "ecto_test",
  username: "postgres",
  password: "postgres",
  hostname: "localhost"
