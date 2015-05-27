defmodule TwitterPlayground.Router do
  use Phoenix.Router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
  end

  scope "/", TwitterPlayground do
    pipe_through :browser
    get "/", PageController, :index
  end

  socket "/ws", TwitterPlayground.Channels do
    channel "tweets", Tweets
  end
end
