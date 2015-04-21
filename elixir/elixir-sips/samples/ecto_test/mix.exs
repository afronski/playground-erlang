defmodule EctoTest.Mixfile do
  use Mix.Project

  def project do
    [app: :ecto_test,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [
        applications: [:logger],
        mod: {EctoTest, []}
    ]
  end

  defp deps do
    [
        {:postgrex, "~> 0.8.0"},
        {:ecto, "~> 0.10.0"}
    ]
  end
end
