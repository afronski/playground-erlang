use Mix.Config

config :json_api, JsonApi.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "CWEnll6YIJFl9rV12Ojpz/P188CaMG7LIxF+Wyt014tlUVXqg/SD8VmOaDjwXOHc",
  debug_errors: false,
  pubsub: [name: JsonApi.PubSub,
           adapter: Phoenix.PubSub.PG2]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

import_config "#{Mix.env}.exs"
