defmodule SdlPlayground.Mixfile do
  use Mix.Project

  def project do
    [app: :sdl_playground,
     version: "0.0.1",
     elixir: "~> 1.0",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [
        {:esdl2, github: "ninenines/esdl2"}
    ]
  end
end
