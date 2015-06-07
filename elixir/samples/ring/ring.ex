defmodule Ring do

  def run do
    run(10, 5, "hi!")
  end

  def run(m, n, message) do
    pid = spawn_link(__MODULE__, :start_process, [ n - 1 ])
    send pid, { :message, message, m }
  end

  # First Process.

  def start_process(count) do
    IO.puts "#{count}"
    pid = spawn_link(__MODULE__, :start_process, [ count - 1, self ])
    loop(pid)
  end

  # Last Process.

  def start_process(0, last) do
    IO.puts "0"
    loop(last)
  end

  # Intermediate Processes.

  def start_process(count, last) do
    IO.puts "#{count}"
    pid = spawn_link(__MODULE__, :start_process, [ count - 1, last ])
    loop(pid)
  end

  def loop(next_pid) do
    receive do
      { :message, message, 0 } ->
        IO.puts "#{inspect self} shutting down. next_pid: #{inspect next_pid}."
        send next_pid, { :message, message, 0 }
        :ok

      { :message, message, m } ->
        IO.puts "m: #{m}, self: #{inspect self}, next_pid: #{inspect next_pid}."
        send next_pid, { :message, message, m - 1 }
        loop(next_pid)
    end
  end
end