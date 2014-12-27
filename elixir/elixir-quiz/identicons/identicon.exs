defmodule Identicon do
  def make(username) do
    username
    |> encode
    |> draw
  end

  defp encode(username) do
    :crypto.hash(:md5, username)
    |> color
    |> alpha
  end

  defp color(md5), do: _color(md5 |> :binary.bin_to_list)
  defp _color([ r, g, b | tail ]), do: { { r, g, b }, tail ++ tail }

  defp alpha({ { r, g, b }, [ a | tail ] }), do: { { r, g, b, a }, tail }

  defp draw({ _, [] }), do: nil
  defp draw({ _, [ a, b, c, d, e | _tail ]}) do
    Enum.flat_map([ a, b, c, d, e ], &([ &1, &1, &1, &1, &1 ]))
    |> Enum.map(&choose_color/1)
    |> render
  end

  defp choose_color(color) when color < 86, do: :red_background
  defp choose_color(color) when color < 172, do: :green_background
  defp choose_color(_), do: :blue_background

  defp render(array) do
    Enum.flat_map(array, &([ &1, " " ]))
    |> IO.ANSI.format(true)
    |> draw_lines
  end

  defp draw_lines(string) do
    IO.puts string
    IO.puts string
    IO.puts string
  end
end

Identicon.make("afronski")