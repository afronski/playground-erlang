defmodule KV.Registry do
  use GenServer

  @doc """
  Starts the registry.
  """
  def start_link(table, event_manager, buckets, opts \\ []) do
    GenServer.start_link(__MODULE__, { table, event_manager, buckets }, opts)
  end

  @doc """
  Looks up the bucket PID for `name` stored in `server`.

  Returns `{ :ok, pid }` if the bucket exists, `:error` otherwise.
  """
  def lookup(table, name) do
    case :ets.lookup(table, name) do
      [ { ^name, bucket } ] -> { :ok, bucket }
      [] -> :error
    end
  end

  @doc """
  Ensures there is a bucket associated to the
  given `name` in `server`.
  """
  def create(server, name) do
    GenServer.call(server, { :create, name })
  end

  @doc """
  Stops the registry.
  """
  def stop(server) do
    GenServer.call(server, :stop)
  end

  ## Server Callbacks.

  def init({ table, events, buckets }) do
    ets = :ets.new(table, [ :named_table, read_concurrency: true ])
    refs = HashDict.new

    { :ok, %{ names: ets, refs: refs, events: events, buckets: buckets } }
  end

  def handle_call({ :lookup, name }, _from, state) do
    { :reply, HashDict.fetch(state.names, name), state }
  end

  def handle_call(:stop, _from, state) do
    { :stop, :normal, :ok, state }
  end

  def handle_call({ :create, name }, _from, state) do
    case lookup(state.names, name) do
      { :ok, pid } ->
        { :reply, pid, state }
      :error ->
        { :ok, pid } = KV.Bucket.Supervisor.start_bucket(state.buckets)
        ref = Process.monitor(pid)
        refs = HashDict.put(state.refs, ref, name)
        :ets.insert(state.names, { name, pid })
        GenEvent.sync_notify(state.events, { :create, name, pid })
        { :reply, pid, %{ state | refs: refs } }
    end
  end

  def handle_info({ :DOWN, ref, :process, pid, _reason }, state) do
    { name, refs } = HashDict.pop(state.refs, ref)
    :ets.delete(state.names, name)
    GenEvent.sync_notify(state.events, { :exit, name, pid })
    { :noreply, %{ state | refs: refs } }
  end

  def handle_info(_message, state) do
    { :noreply, state }
  end
end