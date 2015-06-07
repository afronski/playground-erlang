defmodule TcpPacketInjection do
  use Application

  # TODO: http://www.pythonforpentesting.com/2014/08/tcp-packet-injection-with-python.html
  # TODO: https://github.com/msantos/procket

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = []

    opts = [strategy: :one_for_one, name: TcpPacketInjection.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
