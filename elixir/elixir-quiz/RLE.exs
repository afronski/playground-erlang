defmodule RLE do
  def encode(input) do
    ones = Stream.cycle([ 1 ])

    Stream.zip(letters_stream(input), ones)
      |> Enum.reduce([], &group_by/2)
      |> Enum.reverse
      |> Enum.map(fn { letter, frequency } -> "#{frequency}#{letter}" end)
      |> Enum.join
  end

  def group_by({ ch, n1 }, [ { ch, n2 } | rest ]) do
    [ { ch, n1 + n2 } | rest ]
  end

  def group_by({ ch, n }, rest) do
    [ { ch, n } | rest ]
  end

  def letters_stream(input) do
    String.graphemes(input) |> Enum.take(String.length(input))
  end
end

ExUnit.start()

defmodule RunLengthEncoderTest do
  use ExUnit.Case, async: true

  test "Encoding works with all unique letters" do
    input  = "HORSE"
    output = "1H1O1R1S1E"

    assert RLE.encode(input) == output
  end

  test "Encoding works with all uppercase letters" do
    input  = "JJJTTWPPMMMMYYYYYYYYYVVVVVV"
    output = "3J2T1W2P4M9Y6V"

    assert RLE.encode(input) == output
  end
end