defmodule KVServer do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(Task.Supervisor, [ [ name: KVServer.TaskSupervisor ] ]),
      worker(Task, [ KVServer, :accept, [ 4040 ] ])
    ]

    opts = [
      strategy: :one_for_one,
      name: KVServer.Supervisor
    ]

    Supervisor.start_link(children, opts)
  end

  def accept(port) do
    { :ok, socket } = :gen_tcp.listen(port, [
      :binary,
      packet: :line,
      active: false
    ])

    IO.puts "Accepting connections on port #{port}."

    loop_acceptor(socket)
  end

  defp loop_acceptor(socket) do
    { :ok, client } = :gen_tcp.accept(socket)

    Task.Supervisor.start_child(
      KVServer.TaskSupervisor,
      fn -> serve(client) end
    )

    loop_acceptor(socket)
  end

  defp serve(socket) do
    import Pipe

    message = pipe_matching command, { :ok, command },
      read_line(socket)
      |> KVServer.Command.parse
      |> KVServer.Command.run

    write_line(socket, message)
    serve(socket)
  end

  defp read_line(socket) do
    :gen_tcp.recv(socket, 0)
  end

  defp write_line(socket, message) do
    :gen_tcp.send(socket, format_message(message))
  end

  defp format_message({ :ok, text }), do: text
  defp format_message({ :error, :unknown_command }), do: "UNKNOWN COMMAND\r\n"
  defp format_message({ :error, :not_found }), do: "NOT FOUND\r\n"
  defp format_message({ :error, _ }), do: "ERROR\r\n"
end