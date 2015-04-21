defmodule CurrentWeather.YahooFetcher do
  def fetch(woeid) do
    body = get(woeid)
    temperature = extract_temperature(body)

    temperature
  end

  defp getElement([ {:xmlAttribute, _, _, _, _, _, _, _, value, _} ]), do: value

  defp extract_temperature(body) do
    { xml, _rest } = :xmerl_scan.string(:erlang.bitstring_to_list(body))
    temperature = getElement(:xmerl_xpath.string('/rss/channel/item/yweather:condition/@temp', xml))

    temperature
  end

  defp get(woeid) do
    {:ok, 200, _headers, client} = :hackney.get(url_for(woeid))
    {:ok, body} = :hackney.body(client)

    body
  end

  defp url_for(woeid) do
    base_url <> woeid
  end

  defp base_url do
    "http://weather.yahooapis.com/forecastrss?w="
  end
end
