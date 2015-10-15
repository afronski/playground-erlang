defmodule SeatServer.PageController do
  use SeatServer.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
