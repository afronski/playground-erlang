use Mix.Config

config :json_api, JsonApi.Endpoint,
  secret_key_base: "968swssc6y6Wbi2MsJgEWzRhJGcJxhQMXZ0d9uuU5sxGH2weiy3ynNwlAY3zOCat"

config :json_api, JsonApi.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "json_api_prod"
