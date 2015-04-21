defmodule Integration.HelloPlugIntegrationTest do
  use ExUnit.Case

  setup do
    Plug.Adapters.Cowboy.http HelloPlug, []
    :ok
  end

  test "fetch root gets 'Hello World!'" do
    body = fetch('/')
    assert body == "Hello World!"
  end

  defp fetch(url) do
    base_url = 'http://localhost:4000'
    {:ok, {{_, 200, _}, _, body}} = :httpc.request(base_url ++ url)
    :erlang.iolist_to_binary(body)
  end
end
