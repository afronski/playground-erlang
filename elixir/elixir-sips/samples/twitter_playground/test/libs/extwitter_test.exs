defmodule TwitterPlayground.ExTwitterTest do
  use ExUnit.Case

  setup do
    ExTwitter.configure(
      consumer_key: System.get_env("TWITTER_CONSUMER_KEY"),
      consumer_secret: System.get_env("TWITTER_CONSUMER_SECRET"),
      access_token: System.get_env("TWITTER_ACCESS_TOKEN"),
      access_token_secret: System.get_env("TWITTER_ACCESS_SECRET")
    )
  end

  test "Getting 5 tweets from API for specified user" do
    tweets = ExTwitter.search("afronski", [count: 5])
    IO.inspect Enum.map(tweets, fn(tweet) -> tweet.text end)

    assert Enum.count(tweets) == 5
  end

  test "Streaming data from Twitter" do
    pid = spawn_link(fn ->
      stream = ExTwitter.stream_filter(track: "apple")
      for tweet <- stream do
        IO.puts tweet.text
      end
    end)

    :timer.sleep(2000)
    assert is_pid(pid)

    ExTwitter.stream_control(pid, :stop)
  end
end
