defmodule CaesarCipher do
  def encode(text, offset \\ 13) do
    text
    |> String.codepoints
    |> offsetize(&1, offset)
    |> to_string
  end

  defp offsetize(character, offset) when character in 'A'..'Z' or character in 'a'..'z', do: character + offset
  defp offsetize(character, _), do: character
end

ExUnit.start()

defmodule CaesarCipher do
  use ExUnit.Case, async: true

  test "Encoding works with capital letters" do
    input  = "ABCDEFG"
    output = "BCDEFGH"

    assert CaesarCipher.encode(input, 1) == output
  end

  test "Encoding works with small letters" do
    input  = "abcdefg"
    output = "bcdefgh"

    assert CaesarCipher.encode(input, 1) == output
  end

  test "Encoding works with sentences" do
    input  = "abc def"
    output = "bcd efg"

    assert CaesarCipher.encode(input, 1) == output
  end

  test "By default it is a ROT13" do
    input  = "ROT13ABC"
    output = "EBG13NOP"

    assert CaesarCipher.encode(input, 1) == output
  end
end