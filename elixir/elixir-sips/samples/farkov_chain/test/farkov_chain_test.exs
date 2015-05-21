defmodule FarkovChainTest do
  use ExUnit.Case

  @source "this is some source text"
  @dictionary FarkovChain.Dictionary.new
              |> FarkovChain.Dictionary.parse(@source)

  test "generating dictionaries from the source text" do
    assert FarkovChain.Dictionary.next(@dictionary, "this") == [ "is" ]
    assert FarkovChain.Dictionary.next(@dictionary, "source") == [ "text" ]
  end

  test "getting a few words out based on an input word" do
    assert FarkovChain.Generator.generate_words(@dictionary, "this", 3) == "this is some"
  end
end
