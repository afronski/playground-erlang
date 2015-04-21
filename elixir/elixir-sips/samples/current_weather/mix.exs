defmodule CurrentWeather.Mixfile do
  use Mix.Project

  def project do
    [app: :current_weather,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [applications: [:logger, :inets, :hackney]]
  end

  defp deps do
    [
        {:hackney, "~> 1.0.6"}
    ]
  end
end
