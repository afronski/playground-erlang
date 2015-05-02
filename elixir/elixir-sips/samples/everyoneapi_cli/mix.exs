defmodule EveryoneapiCli.Mixfile do
  use Mix.Project

  def project do
    [app: :everyoneapi_cli,
     version: "0.0.1",
     elixir: "~> 1.0",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     escript: escript,
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  def escript do
    [
        main_module: EveryoneapiCli
    ]
  end

  defp deps do
    [
        {:everyoneapi, "~> 0.0.1"}
    ]
  end
end
