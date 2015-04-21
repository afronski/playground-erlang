defmodule WebsitePipeline.Mixfile do
  use Mix.Project

  def project do
    [app: :website_pipeline,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [
        {:ibrowse, github: "cmullaparthi/ibrowse"},
        {:httpotion, "~> 2.0.0"}
    ]
  end
end
