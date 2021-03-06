defmodule Agenda.Mixfile do
  use Mix.Project

  def project do
    [app: :agenda,
     version: "0.0.1",
     elixir: "~> 1.0",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:logger],
     registered: [Agenda.Worker],
     mod: {Agenda, []}]
  end

  defp deps do
    [
        {:good_times, "~> 1.0.0"}
    ]
  end
end
