defmodule TwitterPlayground.Endpoint do
  use Phoenix.Endpoint, otp_app: :twitter_playground

  plug Plug.Static,
    at: "/", from: :twitter_playground,
    only: ~w(css images js favicon.ico robots.txt)

  if code_reloading? do
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Plug.Logger

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Poison

  plug Plug.MethodOverride
  plug Plug.Head

  plug Plug.Session,
    store: :cookie,
    key: "_twitter_playground_key",
    signing_salt: "TLlRz8Uu",
    encryption_salt: "kJsHZt2h"

  plug :router, TwitterPlayground.Router
end
