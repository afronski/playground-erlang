defmodule DigraphMaps.Intersection do
  defstruct [:map, :name]

  def new(map, name) do
    :digraph.add_vertex(map.digraph, name)
    %DigraphMaps.Intersection{map: map, name: name}
  end
end
