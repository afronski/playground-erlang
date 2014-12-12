defmodule FizzBuzz do
  def transform(value) when rem(value, 15) == 0, do: "FizzBuzz"
  def transform(value) when rem(value, 3) == 0, do: "Fizz"
  def transform(value) when rem(value, 5) == 0, do: "Buzz"
  def transform(value), do: value

  def up_to(n) do
    1 .. n
    |> Enum.to_list
    |> Enum.map(&transform/1)
    |> Enum.join " "
  end
end

IO.puts FizzBuzz.up_to(15)