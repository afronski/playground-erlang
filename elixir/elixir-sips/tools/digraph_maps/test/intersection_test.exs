defmodule DigraphMaps.IntersectionTest do
  use ExUnit.Case
  alias DigraphMaps.Map
  alias DigraphMaps.Intersection

  test "creating an intersection" do
    map = Map.new

    intersection = Intersection.new(map, "Some intersection name")

    assert %Intersection{} = intersection
    assert intersection.name == "Some intersection name"
    assert intersection.map == map
    assert ["Some intersection name"] = :digraph.vertices(map.digraph)
  end
end
