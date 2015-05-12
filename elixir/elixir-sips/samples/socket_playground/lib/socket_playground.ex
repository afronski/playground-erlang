defmodule SocketPlayground do
  def listen(port), do: listen(port, &handler/1)

  def listen(port, handler) do
    IO.puts "Listen on #{port}..."

    Socket.TCP.listen!(port, packet: :line)
    |> accept(handler)
  end

  def accept(listening_socket, handler) do
    IO.puts "Socket accepted."
    socket = Socket.TCP.accept!(listening_socket)

    IO.puts "Connection handled."

    spawn(fn -> handle(socket, handler) end)
    accept(listening_socket, handler)
  end

  def handle(socket, handler) do
    IO.puts "Handling line..."
    incoming = Socket.Stream.recv!(socket)
    socket |> Socket.Stream.send!(handler.(incoming))
    handle(socket, handler)
  end

  defp handler(line), do: String.upcase(line)
end
