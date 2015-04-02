defmodule TcpPacketInjection.Mixfile do
  use Mix.Project

  def project do
    [app: :tcp_packet_injection,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [
        applications: [:logger],
        mod: {TcpPacketInjection, []}
    ]
  end

  defp deps do
    [
        {:procket, github: "msantos/procket"}
    ]
  end
end
