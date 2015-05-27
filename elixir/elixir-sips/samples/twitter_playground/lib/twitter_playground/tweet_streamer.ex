defmodule TwitterPlayground.TweetStreamer do
  require Logger

  def start(query) do
    stream = ExTwitter.stream_filter(track: query)

    for tweet <- stream do
      TwitterPlayground.Endpoint.broadcast!("tweets", "new_tweet", tweet)
    end
  end
end
