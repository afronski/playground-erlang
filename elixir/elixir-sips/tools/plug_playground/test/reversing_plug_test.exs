defmodule ReversingPlugTest do
  use ExUnit.Case, async: true
  use Plug.Test

  @opts ReversingPlug.init([])

  test "returns hello world" do
    conn = conn(:get, "/")
    conn = ReversingPlug.call(conn, @opts)

    assert conn.state == :sent
    assert conn.status == 200
    assert conn.resp_body == "!dlroW olleH"
  end
end
