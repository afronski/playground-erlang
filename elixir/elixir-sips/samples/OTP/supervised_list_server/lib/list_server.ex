defmodule ListServer do
  use GenServer

  require Lager

  ### Public API
  def start_link(list_data_pid) do
    :gen_server.start_link({:local, :list}, __MODULE__, list_data_pid, [])
  end

  def clear do
    Lager.info "clearing"
    :gen_server.cast :list, :clear
  end

  def add(item) do
    Lager.info "adding #{item}"
    :gen_server.cast :list, {:add, item}
  end

  def remove(item) do
    Lager.info "removing #{item}"
    :gen_server.cast :list, {:remove, item}
  end

  def items do
    Lager.info "returning list of items"
    :gen_server.call :list, :items
  end

  def crash do
    Lager.info "crashing"
    :gen_server.cast :list, :crash
  end

  ### GenServer API
  def init(list_data_pid) do
    list = ListData.get_state(list_data_pid)
    {:ok, {list, list_data_pid}}
  end

  def handle_cast(:clear, {_list, list_data_pid}) do
    {:noreply, {[], list_data_pid}}
  end

  def handle_cast({:add, item}, {list, list_data_pid}) do
    {:noreply, {list ++ [item], list_data_pid}}
  end

  def handle_cast({:remove, item}, {list, list_data_pid}) do
    {:noreply, {List.delete(list, item), list_data_pid}}
  end

  def handle_cast(:crash, _) do
    raise RuntimeError, message: "Boom!"
  end

  def handle_call(:items, _from, {list, list_data_pid}) do
    {:reply, list, {list, list_data_pid}}
  end

  def terminate(_reason, {list, list_data_pid}) do
    Lager.warning "terminating"
    ListData.save_state list_data_pid, list
  end
end
