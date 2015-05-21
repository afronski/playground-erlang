defmodule FarkovChain.Dictionary do
  def new do
    HashDict.new
  end

  def parse(dictionary, source) when is_binary(source) do
    parse(dictionary, String.split(source))
  end

  def parse(dictionary, [ word1, word2 | rest ]) do
    value = Dict.get(dictionary, word1, [])
    dictionary = Dict.put(dictionary, word1, [ word2 | value ])

    parse(dictionary, [ word2 | rest ])
  end

  def parse(dictionary, [ _single ]) do
    dictionary
  end

  def next(dictionary, word) do
    Dict.get(dictionary, word)
  end
end
