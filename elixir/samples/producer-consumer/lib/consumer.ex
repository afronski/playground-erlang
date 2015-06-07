defmodule Consumer do
  def start(manager_pid) do
    spawn(fn -> loop(manager_pid) end)
  end

  def loop(manager_pid) do
    receive do
      { :run, work } ->
        IO.puts "                                                 [Consumer] #{inspect self} doing work for #{work} s."
        :timer.sleep(work * 1000)

        IO.puts "                                                 [Consumer] #{inspect self} is now available."
        send manager_pid, { :done, self }
        loop(manager_pid)
    end
  end
end