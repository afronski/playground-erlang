defmodule SlowReverseProfiler do
  import ExProf.Macro

  def run do
    profile do
      SlowReverse.reverse(Enum.to_list(0 .. 1_000))
    end
  end
end

SlowReverseProfiler.run
