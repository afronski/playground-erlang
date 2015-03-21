defmodule JsonApi.Mixfile do
  use Mix.Project

  def project do
    [app: :json_api,
     version: "0.0.1",
     elixir: "~> 1.0",
     elixirc_paths: ["lib", "web"],
     compilers: [:phoenix] ++ Mix.compilers,
     deps: deps]
  end

  def application do
    [mod: {JsonApi, []},
     applications: [:phoenix, :cowboy, :logger]]
  end

  defp deps do
    [{:phoenix, "~> 0.10.0"},
     {:phoenix_ecto, "~> 0.1"},
     {:postgrex, ">= 0.0.0"},
     {:cowboy, "~> 1.0"}]
  end
end
