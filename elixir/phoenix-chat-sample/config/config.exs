# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# Configures the router
config :phoenix, PhoenixChatSample.Router,
  url: [host: "localhost"],
  http: [port: System.get_env("PORT")],
  secret_key_base: "ZzRqRZbnpngZ8jgWNKYcZpfxFz4kOlWIO0TCfpkWur50cnIq60qz34DgAoacHC4np3BMt/NDysfsA/EY/8W/jQ==",
  catch_errors: true,
  debug_errors: false,
  error_controller: PhoenixChatSample.PageController

# Session configuration
config :phoenix, PhoenixChatSample.Router,
  session: [store: :cookie,
            key: "_phoenix_chat_sample_key"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
