require Logger

defmodule Dwitter do
  use Application

  def start(_type, _args) do
    options  = [port: 4000, compress: true, linger: {true, 10}]

    Logger.info "Starting Cowboy on port #{options[:port]}"
    Plug.Adapters.Cowboy.http(Dwitter.Router, [], options)
  end
end
