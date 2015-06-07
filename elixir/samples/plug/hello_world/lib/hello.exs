defmodule HelloPlug do
  import Plug.Conn

  def init(options) do
    options
  end

  def call(connection, _opts) do
    connection
      |> put_resp_content_type("text/plain")
      |> send_resp(200, "Hello, World!")
  end
end

Plug.Adapters.Cowboy.http HelloPlug, []
IO.puts "Running HelloPlug with Cowboy on 'http://localhost:4000'"