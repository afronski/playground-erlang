defmodule DigraphMaps.Road do
  defmodule OneWay do
    defstruct [:map, :edges]

    def new(from, to) do
      edge = :digraph.add_edge(from.map.digraph, from.name, to.name)

      %OneWay{map: from.map, edges: [edge]}
    end
  end

  defmodule TwoWay do
    defstruct [:map, :edges]

    def new(from, to) do
      edge1 = :digraph.add_edge(from.map.digraph, from.name, to.name)
      edge2 = :digraph.add_edge(from.map.digraph, to.name, from.name)

      %TwoWay{map: from.map, edges: [edge1, edge2]}
    end
  end
end
