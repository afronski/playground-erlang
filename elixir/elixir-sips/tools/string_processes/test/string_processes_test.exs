defmodule StringProcessesTest do
  use ExUnit.Case

  setup do
    on_exit fn ->
      :pg2.delete(:string_processes)
    end
  end

  test "if a reverser is in the group, I should receive my string back reversed" do
    StringProcesses.add_process(StringProcesses.Process.Reverser)
    StringProcesses.publish("amirite?")

    assert_receive {StringProcesses.Process.Reverser, "?etirima"}
  end

  test "multiple processes can be added" do
    StringProcesses.add_process(StringProcesses.Process.Reverser)
    StringProcesses.add_process(StringProcesses.Process.Uppercaser)

    StringProcesses.publish("amirite?")

    assert_receive {StringProcesses.Process.Reverser, "?etirima"}
    assert_receive {StringProcesses.Process.Uppercaser, "AMIRITE?"}
  end

  test "process death is handled nicely" do
    StringProcesses.add_process(StringProcesses.Process.Reverser)
    uppercaser = StringProcesses.add_process(StringProcesses.Process.Uppercaser)

    StringProcesses.publish("amirite?")

    assert_receive {StringProcesses.Process.Reverser, "?etirima"}
    assert_receive {StringProcesses.Process.Uppercaser, "AMIRITE?"}

    Process.exit(uppercaser, :ur_dead_to_me)

    StringProcesses.publish("amirite?")

    assert_receive {StringProcesses.Process.Reverser, "?etirima"}
    refute_receive {StringProcesses.Process.Uppercaser, "AMIRITE?"}
  end
end
