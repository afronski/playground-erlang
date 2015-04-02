defmodule SupervisedListServer do
  use Application

  def start(_, _) do
    ListSupervisor.start_link
  end
end
