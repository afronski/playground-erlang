use Mix.Config

config :twitter_playground, TwitterPlayground.Endpoint,
  http: [port: {:system, "PORT"}],
  url: [host: "example.com"]

import_config "prod.secret.exs"
