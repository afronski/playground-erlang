defmodule HttpClientSurvey.Mixfile do
  use Mix.Project

  def project do
    [app: :http_client_survey,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [applications: [:inets, :ibrowse, :hackney, :logger]]
  end

  defp deps do
    [
        {:ibrowse, github: "cmullaparthi/ibrowse"},
        {:hackney, github: "benoitc/hackney"}
    ]
  end
end
