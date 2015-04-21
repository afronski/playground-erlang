defmodule PlugPlayground.Mixfile do
  use Mix.Project

  def project do
    [app: :plug_playground,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [
        {:plug, "~> 0.11.1"},
        {:cowboy, "~> 1.0"}
    ]
  end
end
