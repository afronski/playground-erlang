defmodule SlowReverse do
  require Logger

  def reverse(list) do
    reverse(list, [])
  end

  def reverse([first | rest], acc) do
    Logger.debug "Remaining: #{inspect rest} - #{inspect acc}"

    # Lazily evaluated version:
    # Logger.debug fn -> "Remaining: #{inspect rest} - #{inspect acc}" end

    reverse(rest, [first | acc])
  end

  def reverse([], acc) do
    acc
  end
end
