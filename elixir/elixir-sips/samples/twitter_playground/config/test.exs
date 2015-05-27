use Mix.Config

config :twitter_playground, TwitterPlayground.Endpoint,
  http: [port: 4001],
  server: false

config :logger, level: :warn
