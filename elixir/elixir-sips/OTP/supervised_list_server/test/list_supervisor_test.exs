defmodule ListSupervisorTest do
  use ExUnit.Case

  test "after crash ListServer should be available with old state" do
    ListSupervisor.start_link

    ListServer.add "book"
    ListServer.add "cane"
    ListServer.remove "cane"
    ListServer.crash

    # Ugly way of waiting until process will restart.
    :timer.sleep 100

    assert ListServer.items == ["book"]
  end
end
