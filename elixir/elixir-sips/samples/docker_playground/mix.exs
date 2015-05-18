defmodule DockerPlayground.Mixfile do
  use Mix.Project

  def project do
    [app: :docker_playground,
     version: "0.0.1",
     elixir: "~> 1.0",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:logger, :erldocker]]
  end

  defp deps do
    [
        {:erldocker, github: "proger/erldocker"}
    ]
  end
end
