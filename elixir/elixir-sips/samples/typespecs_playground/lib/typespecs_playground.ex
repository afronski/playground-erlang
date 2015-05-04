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
end
