defmodule Helloer do
  def hola_hola() do
    receive do
      { sender, message } ->
        send sender, "Received: '#{message}'. Thank you!"
        hola_hola
    end
  end
end

helloer_pid = spawn(Helloer, :hola_hola, [])

send helloer_pid, { self, "Here's a message for you!" }
receive do
  message -> IO.puts message
end

send helloer_pid, { self, "Another message for you!" }
receive do
  message -> IO.puts message
end