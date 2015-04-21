defmodule ApexPlayground do
  @derive Access
  defstruct foo: [
    bar: %{
      first: "1",
      second: "2"
    }
  ]
end
