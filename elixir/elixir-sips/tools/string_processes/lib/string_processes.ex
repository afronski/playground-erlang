defmodule StringProcesses do
  def add_process(module) do
    pid = apply(module, :start, [])

    # This call is idempotent.
    :pg2.create(:string_processes)
    :pg2.join(:string_processes, pid)

    pid
  end

  def publish(message) do
    for pid <- :pg2.get_members(:string_processes) do
      send(pid, {self, message})
    end
  end
end
