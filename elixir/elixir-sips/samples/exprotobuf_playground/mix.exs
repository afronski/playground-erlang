defmodule ExprotobufPlayground.Mixfile do
  use Mix.Project

  def project do
    [app: :exprotobuf_playground,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [
        {:exprotobuf, "~> 0.8.0"},
        {:gpb, "~> 3.16.0"}
    ]
  end
end
