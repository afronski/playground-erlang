defmodule TwitterPlayground.PageController do
  use TwitterPlayground.Web, :controller

  plug :action

  def index(conn, _params) do
    render conn, "index.html"
  end
end
