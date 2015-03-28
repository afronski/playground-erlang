defmodule ListServerTest do
  use ExUnit.Case

  # Clear the list server before each test.
  setup do
    {:ok, list_data_pid} = ListData.start_link
    ListServer.start_link list_data_pid
    ListServer.clear
  end

  test "it starts out empty" do
    assert ListServer.items == []
  end

  test "it lets us add things to the list" do
    ListServer.add "book"
    assert ListServer.items == ["book"]
  end

  test "it lets us remove things from the list" do
    ListServer.add "book"
    ListServer.add "magazine"

    ListServer.remove "book"
    assert ListServer.items == ["magazine"]
  end
end
