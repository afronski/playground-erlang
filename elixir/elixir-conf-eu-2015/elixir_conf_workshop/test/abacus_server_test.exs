defmodule AbacusServerTest do
  use ExUnit.Case

  test "server should know how to add" do
    {:ok, pid} = AbacusServer.start_link()

    assert AbacusServer.add(pid, 2, 2) == 4
  end

  test "server should return its executed operation history" do
    {:ok, pid} = AbacusServer.start_link()

    AbacusServer.add(pid, 3, 3)
    AbacusServer.add(pid, 2, 2)

    result = AbacusServer.get_history(pid)

    assert Enum.count(result) == 2
    assert [ {{:addition, 3, 3}, 6} | _rest ] = result
  end

  test "server should know how to subtract" do
    {:ok, pid} = AbacusServer.start_link()

    assert AbacusServer.subtract(pid, 2, 2) == 0
  end

  test "server should know how to multiply" do
    {:ok, pid} = AbacusServer.start_link()

    assert AbacusServer.multiply(pid, 2, 4) == 8
  end

  test "server should know how to divide" do
    {:ok, pid} = AbacusServer.start_link()

    assert AbacusServer.divide(pid, 1, 2) == 0.5
  end

  test "server should know return an error" do
    {:ok, pid} = AbacusServer.start_link()

    assert AbacusServer.divide(pid, 1, 0) == :error
  end

  test "server should know how to stop itself" do
    {:ok, pid} = AbacusServer.start_link()
    AbacusServer.stop(pid)

    :timer.sleep 100
    refute Process.alive?(pid)
  end
end
