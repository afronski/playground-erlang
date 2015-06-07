defmodule Helloer do
  def hola(message) do
    IO.puts "Hola! #{message}."
  end

  def hola_hola() do
    receive do
      { sender, message } ->
        send sender, "Received: '#{message}'. Thank you!"
    end
  end
end

IO.puts "Parent process is #{inspect self}."
IO.puts "Spawned process is #{inspect spawn(Helloer, :hola, [ 'Elixir is awesome' ])}."

helloer_pid = spawn(Helloer, :hola_hola, [])
send helloer_pid, { self, "Here's a message for you!" }

receive do
  message -> IO.puts message
end