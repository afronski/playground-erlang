defmodule PcmPlayground.Mixfile do
  use Mix.Project

  def project do
    [app: :pcm_playground,
     version: "0.0.1",
     test_coverage: [tool: Coverex.Task],
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [
        {:coverex, "~> 1.2.0"}
    ]
  end
end
