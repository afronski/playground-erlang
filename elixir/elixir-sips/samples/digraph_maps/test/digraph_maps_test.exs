defmodule DigraphMapsTest do
  use ExUnit.Case

  alias DigraphMaps.Map
  alias DigraphMaps.Intersection
  alias DigraphMaps.Road.OneWay
  alias DigraphMaps.Road.TwoWay

  test "four intersections can be connected" do
    map = Map.new

    ra_9 = Intersection.new(map, "Richard Arrington and 9th Avenue")
    ra_aw = Intersection.new(map, "Richard Arrington and Abraham Woods")
    ts_aw = Intersection.new(map, "22nd and Abraham Woods")
    ts_9 = Intersection.new(map, "22nd and 9th Avenue")

    ra = OneWay.new(ra_aw, ra_9)
    ninth = TwoWay.new(ra_9, ts_9)

    _ts = OneWay.new(ts_9, ts_aw)
    _aw = TwoWay.new(ts_aw, ra_aw)

    assert ra_9.map == map
    assert %Intersection{} = ra_9

    assert ra.map == map
    assert %OneWay{} = ra
    assert %TwoWay{} = ninth

    assert Map.shortest_path(ra_9, ra_aw) == ["Richard Arrington and 9th Avenue",
                                              "22nd and 9th Avenue",
                                              "22nd and Abraham Woods",
                                              "Richard Arrington and Abraham Woods"]
    assert Map.shortest_path(ra_aw, ra_9) == ["Richard Arrington and Abraham Woods",
                                              "Richard Arrington and 9th Avenue"]
  end
end
