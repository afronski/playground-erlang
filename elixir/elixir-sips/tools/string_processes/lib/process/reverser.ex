defmodule StringProcesses.Process.Reverser do
  def start do
    spawn(__MODULE__, :await, [])
  end

  def await do
    receive do
      {pid, message} -> send(pid, {__MODULE__, String.reverse(message)})
    end
    await
  end
end
