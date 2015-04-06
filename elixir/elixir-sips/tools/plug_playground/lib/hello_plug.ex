defmodule HelloPlug do
  import Plug.Conn

  def init(options) do
    # Here's where you would initialize any options.

    options
  end

  def call(conn, _opts) do
    conn
    |> put_resp_content_type("text/plain")
    |> send_resp(200, "Hello World!")
  end
end
