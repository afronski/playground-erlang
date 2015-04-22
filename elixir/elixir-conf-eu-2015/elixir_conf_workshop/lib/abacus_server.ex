defmodule AbacusServer do
  use GenServer

  # Public API.

  # We can use here a named process (but we will receive a singleton,
  # it depends on the context if it is good or not).
  #
  # def start_link(history) do
  #   GenServer.start_link(__MODULE__, history, [{:name, __MODULE__}])
  # end
  #
  # Then, we don't have to remember `pid` and pass it
  # everywhere. Instead, we can use only module name e.g.:
  #
  # GenServer.call(__MODULE__, {:add, a, b})

  def start_link() do
    GenServer.start_link(__MODULE__, [], [])
  end

  def start_link(history) do
    GenServer.start_link(__MODULE__, history, [])
  end

  def add(pid, a, b) do
    GenServer.call(pid, {:addition, a, b})
  end

  def subtract(pid, a, b) do
    GenServer.call(pid, {:subtraction, a, b})
  end

  def multiply(pid, a, b) do
    GenServer.call(pid, {:multiplication, a, b})
  end

  def divide(pid, a, b) do
    GenServer.call(pid, {:division, a, b})
  end

  def get_history(pid) do
    GenServer.call(pid, :get_history)
  end

  def stop(pid) do
    GenServer.cast(pid, :stop)
  end

  # GenServer API.

  def handle_call({:addition, a, b} = message, _, history) do
    result = Abacus.addition(a, b)
    {:reply, result, store_history(message, result, history)}
  end

  def handle_call({:subtraction, a, b} = message, _, history) do
    result = Abacus.subtraction(a, b)
    {:reply, result, store_history(message, result, history)}
  end

  def handle_call({:multiplication, a, b} = message, _, history) do
    result = Abacus.multiplication(a, b)
    {:reply, result, store_history(message, result, history)}
  end

  def handle_call({:division, a, b} = message, _, history) do
    result = try do
               Abacus.division(a, b)
             rescue
               _ -> :error
             end

    {:reply, result, store_history(message, result, history)}
  end

  def handle_call(:get_history, _, history) do
    {:reply, Enum.reverse(history), history}
  end

  def handle_cast(:stop, history) do
    {:stop, :normal, history}
  end

  # Private implementation.

  defp store_history(message, result, history) do
    [ {message, result} | history ]
  end
end
