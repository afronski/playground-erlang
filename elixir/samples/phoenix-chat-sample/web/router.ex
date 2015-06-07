defmodule PhoenixChatSample.Router do
  use Phoenix.Router
  use Phoenix.Router.Socket, mount: "/ws"

  pipeline :browser do
    plug :accepts, ~w(html)
    plug :fetch_session
  end

  pipeline :api do
    plug :accepts, ~w(json)
  end

  scope "/" do
    pipe_through :browser

    get "/", PhoenixChatSample.PageController, :index
  end

  channel "room", PhoenixChatSample.RoomChannel
end
