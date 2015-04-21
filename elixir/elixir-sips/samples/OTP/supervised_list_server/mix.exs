Code.append_path "_build/shared/lib/relex/ebin"
Code.append_path "_build/shared/lib/pogo/ebin"

if Code.ensure_loaded?(Relex.Release) do
  defmodule SupervisedListServer.Release do
    use Relex.Release
    use Pogo.Release

    def name, do: "supervised_list_server"
    def applications, do: [:pogo, :supervised_list_server]
  end
end

defmodule SupervisedListServer.Mixfile do
  use Mix.Project

  def project do
    [app: :supervised_list_server,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps,
     release: SupervisedListServer.Release
    ]
  end

  def application do
    [
        applications: [:exlager],
        mod: {SupervisedListServer, []}
    ]
  end

  defp deps do
    [
        {:exlager, github: "khia/exlager"},
        {:relex, github: "interline/relex", branch: "start_clean"},
        {:pogo, github: "onkel-dirtus/pogo"}
    ]
  end
end
