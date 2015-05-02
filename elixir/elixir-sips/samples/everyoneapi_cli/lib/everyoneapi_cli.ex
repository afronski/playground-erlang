defmodule EveryoneapiCli do
  def main(number) do
    Everyoneapi.start
    IO.inspect Everyoneapi.info! number
  end
end
