defmodule SupervisedListServer.Mixfile do
  use Mix.Project

  def project do
    [app: :supervised_list_server,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [
        applications: [:exlager],
        mod: {SupervisedListServer, []}
    ]
  end

  defp deps do
    [
        {:exlager, github: "khia/exlager"}
    ]
  end
end
