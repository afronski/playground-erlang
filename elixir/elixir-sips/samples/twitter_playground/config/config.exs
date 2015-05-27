use Mix.Config

config :twitter_playground, TwitterPlayground.Endpoint,
  url: [host: "localhost"],
  root: Path.expand("..", __DIR__),
  secret_key_base: "rYqxEk8JeXCfX9HwSGz9eqiNxXsZWzaeSAXtH/IWs7MZf0cv2+jvnrLcUHgeW1Vm",
  debug_errors: false,
  pubsub: [name: TwitterPlayground.PubSub,
           adapter: Phoenix.PubSub.PG2]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

import_config "#{Mix.env}.exs"
