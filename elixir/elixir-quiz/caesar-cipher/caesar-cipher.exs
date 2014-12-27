defmodule CaesarCipher do
  def encode_file(path, offset \\ 13) do
    path
    |> File.read!
    |> encode(offset)
  end

  def encode(text, offset \\ 13)

  def encode(_, offset) when offset < 0, do: raise "Offset has to be positive."
  def encode(_, offset) when offset >= 26, do: raise "Offset has to be less than 26."

  def encode(text, offset) do
    text
    |> String.upcase
    |> String.codepoints
    |> parallel_map(&extract_code/1)
    |> parallel_map(&offsetize(&1, offset))
    |> parallel_map(&encode_point/1)
    |> Enum.join
  end

  defp extract_code(codepoint) do
    << point >> = codepoint
    point
  end

  defp offsetize(character, offset) when character in 65..90, do: character + offset
  defp offsetize(character, _), do: character

  defp encode_point(point) when point > 90, do: << point - 26 >>
  defp encode_point(point), do: << point >>

  defp parallel_map(collection, callback) do
    me = self

    collection
    |> Enum.map(fn i -> spawn_link fn -> send me, { self, callback.(i) } end end)
    |> Enum.map(fn pid -> receive do { ^pid, result } -> result end end)
  end
end

ExUnit.start()

defmodule CaesarCipherTest do
  use ExUnit.Case, async: true

  test "Encoding works with capital letters" do
    input  = "ABCDEFG"
    output = "BCDEFGH"

    assert CaesarCipher.encode(input, 1) == output
  end

  test "Encoding works with small letters" do
    input  = "abcdefg"
    output = "BCDEFGH"

    assert CaesarCipher.encode(input, 1) == output
  end

  test "Encoding works with sentences" do
    input  = "abc def"
    output = "BCD EFG"

    assert CaesarCipher.encode(input, 1) == output
  end

  test "Testing end of alphabet" do
    input  = "xyzXYZ"
    output = "YZAYZA"

    assert CaesarCipher.encode(input, 1) == output
  end

  test "By default it is a ROT13" do
    input  = "ROT13ABC"
    output = "EBG13NOP"

    assert CaesarCipher.encode(input) == output
  end

  test "File encoding should work properly" do
    input_file = "input.txt"
    output = "EBG13NOP"

    assert CaesarCipher.encode_file(input_file) == output
  end
end