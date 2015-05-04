defmodule TypespecsPlayground do
  defmodule Add do
    @type t :: %Add{first: number, second: number}
    defstruct first: 0, second: 0
  end

  @doc """
  Add two numbers together.
  """
  @spec add(number, number) :: number
  def add(first, second) do
    first + second
  end

  @spec add(list(number)) :: number
  def add(numbers) when is_list(numbers) do
    List.foldl(numbers, 0, fn(x, acc) -> x + acc end)
  end

  @spec add(Add.t) :: number
  def add(%Add{first: first, second: second}) do
    first + second
  end

  defmodule Bad do
    @spec add(number, number) :: String.t
    def add(first, second) do
      TypespecsPlayground.add(first, second)
    end

    def crazy(maybe_atom) do
      Dict.fetch(maybe_atom, :foo)
      Atom.to_string(maybe_atom)
    end
  end
end
