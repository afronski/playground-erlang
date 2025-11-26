defmodule AwsNewsComLaunchesExtractor.MixProject do
  use Mix.Project

  def project do
    [
      app: :aws_news_com_launches_extractor,
      version: "1.0.0",
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:fast_rss, "~> 0.5.0"},
      {:req, "~> 0.5.0"},
      {:timex, "~> 3.7.0"},
      {:csv, "~> 3.2.0"},
      {:floki, "~> 0.38.0"},
      {:html_sanitize_ex, "~> 1.4"}
    ]
  end
end
