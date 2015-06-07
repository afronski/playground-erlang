defmodule PhoenixChatSample.RoomChannel do
  use Phoenix.Channel

  def join(socket, "sample_chat", _data) do
    IO.puts "Someone has joined!"

    { :ok, socket }
  end

  def event(socket, "new:message", data) do
    IO.puts "Received new message from the GUI."

    broadcast socket, "new:message", data

    socket
  end
end