defmodule AbacusApplicationTest do
  use ExUnit.Case

  test "application should register server under name `:abacus`" do
    AbacusApplication.start(:normal, [])

    assert AbacusServer.add(:abacus, 2, 2) == 4
  end
end
