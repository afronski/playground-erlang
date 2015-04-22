defmodule AbacusSupervisorTest do
  use ExUnit.Case

  test "supervisor should return a PID to an abacus server" do
    {:ok, pid} = AbacusSupervisor.start_link([])

    assert AbacusServer.add(pid, 2, 2) == 4
  end

  test "supervisor should pass initial state to the server" do
    {:ok, pid} = AbacusSupervisor.start_link([ {{:addition, 2, 2}, 4} ])

    assert Enum.count(AbacusServer.get_history(pid)) == 1
  end
end
