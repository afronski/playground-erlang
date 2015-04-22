defmodule AbacusTest do
  use ExUnit.Case

  test "having an abacus I can do addition" do
    assert Abacus.addition(2, 2) == 4
    assert Abacus.addition(0, -2) == -2
    assert Abacus.addition(0, 0) == 0
  end

  test "having an abacus I can do subtraction" do
    assert Abacus.subtraction(2, 2) == 0
    assert Abacus.subtraction(0, 2) == -2
    assert Abacus.subtraction(-2, 2) == -4
  end

  test "having an abacus I can do multiplication" do
    assert Abacus.multiplication(2, 2) == 4
    assert Abacus.multiplication(2, 0) == 0
    assert Abacus.multiplication(2, 1) == 2
  end

  test "having an abacus I can do division" do
    assert Abacus.division(2, 1) == 2
    assert Abacus.division(2, 2) == 1

    assert_raise ArithmeticError, fn ->
      Abacus.division(2, 0)
    end
  end
end
