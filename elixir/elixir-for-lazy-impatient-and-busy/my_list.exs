defmodule MyList do
  def empty?([]), do: true
  def empty?(list) when is_list(list) do
    false
  end

  def first([ head | _tail ]), do: head

  def count([]), do: 0
  def count([ _head | tail ]) do
    1 + count(tail)
  end

  def flatten([]), do: []
  def flatten([ head | tail ]) do
    flatten(head) ++ flatten(tail)
  end
  def flatten(head), do: [ head ]
end

IO.puts MyList.empty? [ 1, 2, 3 ]
IO.puts MyList.empty? []

IO.puts MyList.first [ 1 ]
IO.puts MyList.first [ 3, 2, 1, 6, 7 ]

IO.puts MyList.count []
IO.puts MyList.count [ 1, 2, 3 ]

IO.inspect MyList.flatten [ 1, 2, 3 ]
IO.inspect MyList.flatten [ 1, [ 2, [], [ 3, [ 4 ] ] ], 5 ]