defmodule ErlcloudPlayground.Mixfile do
  use Mix.Project

  def project do
    [app: :erlcloud_playground,
     version: "0.0.1",
     elixir: "~> 1.0",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:logger, :ssl, :erlcloud]]
  end

  defp deps do
    [
        {:erlcloud, "~> 0.9.2"}
    ]
  end
end
