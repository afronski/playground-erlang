defmodule WebsitePipeline do
  def map_titles(sites) do
    sites
    |> Enum.map(fn(url) ->
      Task.async(fn -> url |> get_body |> extract_title end)
    end)
    |> Enum.map(&Task.await/1)
  end

  defp get_body(url) do
    IO.puts url
    HTTPotion.get(url).body
  end

  defp extract_title(nil), do: ""
  defp extract_title(html) do
    title_pattern = ~r"<title>([^<]*)</title>"
    Regex.run(title_pattern, html) |> Enum.at(1)
  end
end
