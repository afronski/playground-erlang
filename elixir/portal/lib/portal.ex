defmodule Portal do
  use Application

  defstruct [ :left, :right ]

  @doc """
  Starts transfering `data` from `left` to `right`.
  """
  def transfer(left, right, data) do
    # First add all data to the portal on the left.
    for item <- data do
      Portal.Door.push(left, item)
    end

    %Portal{ left: left, right: right }
  end

  @doc """
  Pushes data to the right in the given `portal`.
  """
  def push_right(portal) do
    # See if we can pop data from left. If so, push the
    # popped data to the right, Otherwise, do nothing.
    case Portal.Door.pop(portal.left) do
      :error     -> :ok
      { :ok, h } -> Portal.Door.push(portal.right, h)
    end

    # Let's return portal itself...
    portal
  end

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(Portal.Door, [])
    ]

    opts = [
      strategy: :simple_one_for_one,
      name: Portal.Supervisor
    ]

    Supervisor.start_link(children, opts)
  end

  @doc """
  Shoots a new door with the given `color`.
  """
  def shoot(color) do
    Supervisor.start_child(Portal.Supervisor, [ color ])
  end
end

defimpl Inspect, for: Portal do
  def inspect(%Portal{ left: left, right: right }, _) do
    left_door = inspect(left)
    right_door = inspect(right)

    left_data = inspect(Enum.reverse(Portal.Door.get(left)))
    right_data = inspect(Portal.Door.get(right))

    max = max(String.length(left_door), String.length(left_data))

    """
    #Portal<
      #{String.rjust(left_door, max)} <=> #{right_door}
      #{String.rjust(left_data, max)} <=> #{right_data}
    >
    """
  end
end