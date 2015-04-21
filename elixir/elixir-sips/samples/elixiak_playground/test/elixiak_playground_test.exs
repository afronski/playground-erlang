defmodule ElixiakPlaygroundTest do
  use ExUnit.Case

  setup do
    Riak.start
    Riak.configure(host: '127.0.0.1', port: 8087)

    delete_all_logs

    :ok
  end

  test "we can store logs" do
    Log.create(application: "web", content: "GET /foo/bar by 10.0.0.1").save!
    Log.create(application: "backend", content: "Image foo1.jpg resize").save!

    web_results = Log.find(application: "web")
    assert Enum.count(web_results) == 1
    assert List.last(web_results).content == "GET /foo/bar by 10.0.0.1"
  end

  defp delete_all_logs do
    {:ok, keys} = Riak.Bucket.keys Log.bucket
    keys |> Enum.each(fn(key) -> Riak.delete(Log.bucket, key) end)
  end
end
