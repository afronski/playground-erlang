defmodule BoltunPlayground.Mixfile do
  use Mix.Project

  def project do
    [app: :boltun_playground,
     version: "0.0.1",
     elixir: "~> 1.0",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:logger, :boltun]]
  end

  defp deps do
    [
        {:boltun, "~> 0.0.4"}
    ]
  end
end
