defmodule SoapWeather.Mixfile do
  use Mix.Project

  def project do
    [app: :soap_weather,
     version: "0.0.1",
     elixir: "~> 1.0",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [
        {:erlsom, github: "willemdj/erlsom"},
        {:detergentex, "~> 0.0.3"}
    ]
  end
end
