defmodule TwitterPlayground.PageControllerTest do
  use TwitterPlayground.ConnCase

  test "GET /" do
    conn = get conn(), "/"
    assert conn.resp_body =~ "Hello Phoenix!"
  end
end
