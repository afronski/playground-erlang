defmodule FarkovChain.Generator do
  def generate_words(dictionary, start_word, num_words) do
    generate_words(dictionary, start_word, num_words - 1, [ start_word ])
  end

  defp generate_words(_dictionary, _start_word, 0, result) do
    Enum.reverse(result) |> Enum.join(" ")
  end
  defp generate_words(dictionary, start_word, num_words, generated_words) do
    new_word = get_word(dictionary, start_word)
    generate_words(dictionary, new_word, num_words - 1, [ new_word | generated_words ])
  end

  defp get_word(dictionary, start_word) do
    case FarkovChain.Dictionary.next(dictionary, start_word) do
      nil -> nil
      list -> Enum.shuffle(list) |> hd
    end
  end
end
