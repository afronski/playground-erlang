defmodule FridgeServerTest do
  use ExUnit.Case

  test "putting something into the fridge" do
    fridge = FridgeServer.start_link

    assert :ok == FridgeServer.store fridge, :bacon
  end

  test "removing something from the fridge" do
    fridge = FridgeServer.start_link

    FridgeServer.store fridge, :bacon
    assert {:ok, :bacon} == FridgeServer.take fridge, :bacon
  end

  test "taking something from the fridge that isn't in there" do
    fridge = FridgeServer.start_link

    assert :not_found == FridgeServer.take fridge, :bacon
  end

  test "removing should actually remove item from the fridge" do
    fridge = FridgeServer.start_link

    FridgeServer.store fridge, :bacon
    FridgeServer.take fridge, :bacon

    assert :not_found == FridgeServer.take fridge, :bacon
  end
end
