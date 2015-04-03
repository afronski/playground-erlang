defmodule ShouldiBlacksmithPlayground.Mixfile do
  use Mix.Project

  def project do
    [app: :shouldi_blacksmith_playground,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [applications: applications(Mix.env)]
  end

  defp applications(:test), do: applications(:all) ++ [:blacksmith]
  defp applications(_all), do: [:logger, :postgrex, :ecto]

  defp deps do
    [
        {:postgrex, "~> 0.8"},
        {:ecto, "~> 0.10"},
        {:shouldi, "~> 0.2", only: :test},
        {:blacksmith, "~> 0.1"}
    ]
  end
end
