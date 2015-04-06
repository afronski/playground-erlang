defmodule ReversingPlug do
  use Plug.Builder
  import Plug.Conn

  plug :hello
  plug :reverse
  plug :sender

  def hello(conn, _opts) do
    assign(conn, :resp_body, "Hello World!")
  end

  def reverse(conn, _opts) do
    assign(conn, :resp_body, String.reverse(conn.assigns[:resp_body]))
  end

  def sender(conn, _opts) do
    conn
    |> put_resp_content_type("text/plain")
    |> send_resp(200, conn.assigns[:resp_body])
  end
end
