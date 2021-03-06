defmodule Producer do
  @timeout 2000

  def start(manager_pid) do
    spawn(fn -> run(manager_pid) end)
  end

  def run(manager_pid) do
    Stream.repeatedly(fn ->
      IO.puts "[Producer] Request for consumers."
      send manager_pid, { :request, self }

      receive do
        { :ok, consumer_pid } ->
          IO.puts "[Producer] Sending work to consumer #{inspect consumer_pid}."
          :random.seed(:erlang.now)
          send consumer_pid, { :run, :random.uniform(3) }

      after @timeout ->
        IO.puts "[Producer] Waiting for free consumers."
        send manager_pid, { :request, self }
      end
    end) |> Enum.to_list
  end
end