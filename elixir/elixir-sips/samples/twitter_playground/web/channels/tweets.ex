defmodule TwitterPlayground.Channels.Tweets do
  use Phoenix.Channel
  require Logger

  alias TwitterPlayground.TweetStreamer

  def join("tweets", %{ "query" => query }, socket) do
    Logger.info "Joining Tweets channel with '#{query}'"

    spawn_link(fn () ->
      TweetStreamer.start(query)
    end)

    {:ok, socket}
  end

  def handle_out("new_tweet", payload, socket) do
    Logger.info "Payload: #{payload.text}"

    push socket, "new_tweet", payload
    {:noreply, socket}
  end
end
