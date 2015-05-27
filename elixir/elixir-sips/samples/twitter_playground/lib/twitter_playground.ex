defmodule TwitterPlayground do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    ExTwitter.configure(
      consumer_key: System.get_env("TWITTER_CONSUMER_KEY"),
      consumer_secret: System.get_env("TWITTER_CONSUMER_SECRET"),
      access_token: System.get_env("TWITTER_ACCESS_TOKEN"),
      access_token_secret: System.get_env("TWITTER_ACCESS_SECRET")
    )

    children = [
      supervisor(TwitterPlayground.Endpoint, []),
    ]

    opts = [strategy: :one_for_one, name: TwitterPlayground.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def config_change(changed, _new, removed) do
    TwitterPlayground.Endpoint.config_change(changed, removed)
    :ok
  end
end
