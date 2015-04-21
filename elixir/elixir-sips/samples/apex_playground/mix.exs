defmodule ApexPlayground.Mixfile do
  use Mix.Project

  def project do
    [app: :apex_playground,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [
        {:apex, "~> 0.3.2"}
    ]
  end
end
