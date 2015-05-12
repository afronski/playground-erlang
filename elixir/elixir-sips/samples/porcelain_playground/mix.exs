defmodule PorcelainPlayground.Mixfile do
  use Mix.Project

  def project do
    [app: :porcelain_playground,
     version: "0.0.1",
     elixir: "~> 1.0",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:logger, :porcelain]]
  end

  defp deps do
    [
        {:porcelain, "~> 2.0.0"}
    ]
  end
end
