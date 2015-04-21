defmodule OddProtocolTest do
  use ExUnit.Case

  test "integers know if they're odd" do
    assert Odd.odd?(1)
    refute Odd.odd?(2)
  end

  test "floats are odd if their floor is odd" do
    assert Odd.odd?(1.1)
    assert Odd.odd?(1.9)
    refute Odd.odd?(2.1)
  end

  test "lists are odd if they have an odd number of elements" do
    refute Odd.odd?([])
    assert Odd.odd?([1])
  end

  test "animals are odd if they're hairy" do
    assert Odd.odd?(%Animal{hairy: true})
    refute Odd.odd?(%Animal{hairy: false})
  end

  test "unspecified things aren't odd" do
    refute Odd.odd?(%{})
    refute Odd.odd?("jabow")
    refute Odd.odd?(:poland)
  end
end
