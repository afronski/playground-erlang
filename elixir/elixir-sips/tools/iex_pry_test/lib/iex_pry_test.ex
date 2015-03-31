require IEx

defmodule IexPryTest do
  def add(a, b) do
    c = a + b

    # Inspecting running process.
    # After invoking that and accepting the request,
    # you will have access to the whole lexical scope
    # of actually running process.
    IEx.pry

    c
  end
end
