defmodule SoapWeatherTest do
  use ExUnit.Case

  @wsdl "http://www.webservicex.net/globalweather.asmx?WSDL"
  test "obtaining list of cities" do
    {:ok, _, [ {response_type, _, data}]} = Detergentex.call(@wsdl, "GetCitiesByCountry", ["Poland"])

    assert response_type == :'p:GetCitiesByCountryResponse'
    assert String.contains?(:erlang.iolist_to_binary(data), "Katowice")
  end

  test "obtaining the weather in Katowice" do
    {:ok, _, [ {response_type, _, data}]} = Detergentex.call(@wsdl, "GetWeather", ["Katowice", "Poland"])

    weather = :erlsom.simple_form(data)

    assert response_type == :'p:GetWeatherResponse'
    IO.inspect(weather)
  end
end
