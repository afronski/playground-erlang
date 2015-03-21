defmodule JsonApi.Endpoint do
  use Phoenix.Endpoint, otp_app: :json_api

  plug Plug.Static,
    at: "/", from: :json_api,
    only: ~w(css images js favicon.ico robots.txt)

  plug Plug.Logger
  plug Phoenix.CodeReloader

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Poison

  plug Plug.MethodOverride
  plug Plug.Head

  plug Plug.Session,
    store: :cookie,
    key: "_json_api_key",
    signing_salt: "pV8dqHKK",
    encryption_salt: "4iIYVT3f"

  plug :router, JsonApi.Router
end
