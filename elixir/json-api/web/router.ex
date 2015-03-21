defmodule JsonApi.Router do
  use Phoenix.Router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", JsonApi do
    pipe_through :api

    resources "/contacts", ContactController
  end
end
