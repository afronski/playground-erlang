defmodule ErlangJsPlaygroundTest do
  use ExUnit.Case

  test "getting data out of the JavaScript interpreter" do
    :application.ensure_all_started(:erlang_js)
    {:ok, js} = :js_driver.new

    :ok = :js.define(js, "var addOne = function(n) {return n+1; };")
    {:ok, 3} = :js.call(js, "addOne", [2])
  end
end
