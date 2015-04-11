defmodule DigraphMaps.Map do
  defstruct [:digraph]

  def new do
    %DigraphMaps.Map{digraph: :digraph.new}
  end

  def shortest_path(from, to) do
    :digraph.get_short_path(from.map.digraph, from.name, to.name)
  end
end
