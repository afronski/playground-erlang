defmodule TestListener do
  use Boltun, otp_app: :boltun_playground
  require Logger

  listen do
    channel "elixirsips", :handle_elixirsips
  end

  def handle_elixirsips("elixirsips", payload) do
    Logger.debug "Handling 'elixirsips'."
    Logger.debug inspect payload
  end
end
