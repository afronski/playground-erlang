defmodule FizzBuzz do
  @doc """
  Print the FizzBuzz sequence from 1 to `n`

  ## Example

    iex> FizzBuzz.up_to 20
    1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 Fizz Buzz 16 17 Fizz 19 Buzz
  """

  def up_to(n) do
    fizzbuzz_stream |> Enum.take(n) |> Enum.join(" ") |> IO.puts
  end

  @doc """
  Return a stream of FizzBuzz values.
  """
  def fizzbuzz_stream do
    threes = Stream.cycle [ nil, nil, "Fizz" ]
    fives = Stream.cycle [ nil, nil, nil, nil, "Buzz" ]
    Stream.zip(threes, fives) |> Stream.with_index |> Stream.map(&speak/1)
  end

  defp speak({ { nil, nil }, n }), do: "#{n + 1}"
  defp speak({ { fizz, buzz }, _ }), do: "#{fizz}#{buzz}"
end

FizzBuzz.up_to(15)