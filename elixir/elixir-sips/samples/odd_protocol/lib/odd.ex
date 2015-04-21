defprotocol Odd do
  @fallback_to_any true

  @doc "Returns true if data is considered odd"
  def odd?(data)
end

defimpl Odd, for: Any do
  def odd?(_), do: false
end

defimpl Odd, for: Integer do
  require Integer

  def odd?(data) do
    Integer.is_odd(data)
  end
end

defimpl Odd, for: Float do
  def odd?(data) do
    Odd.odd?(Kernel.trunc(Float.floor(data)))
  end
end

defimpl Odd, for: List do
  require Integer

  def odd?(data) do
    Odd.odd?(Enum.count(data))
  end
end

defmodule Animal do
  defstruct hairy: false, leg_count: 4
end

defimpl Odd, for: Animal do
  def odd?(%Animal{hairy: true}), do: true
  def odd?(_), do: false
end
