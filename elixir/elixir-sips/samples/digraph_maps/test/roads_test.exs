defmodule DigraphMaps.RoadsTest do
  use ExUnit.Case
  alias DigraphMaps.Map
  alias DigraphMaps.Intersection
  alias DigraphMaps.Road.OneWay
  alias DigraphMaps.Road.TwoWay

  test "creating a one way road" do
    map = Map.new

    first_int = Intersection.new(map, "First")
    second_int = Intersection.new(map, "Second")

    road = OneWay.new(first_int, second_int)

    assert %OneWay{} = road
    assert road.map == map
    assert Enum.count(road.edges) == 1
  end

  test "creating a two way road" do
    map = Map.new

    first_int = Intersection.new(map, "First")
    second_int = Intersection.new(map, "Second")

    road = TwoWay.new(first_int, second_int)

    assert %TwoWay{} = road
    assert road.map == map
    assert Enum.count(road.edges) == 2
  end
end
