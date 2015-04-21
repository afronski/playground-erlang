defmodule ScreamTest do
  use ExUnit.Case

  test "screaming" do
    Mix.Tasks.Scream.run(["zomg"])

    assert_received {:mix_shell, :info, ["ZOMG"]}
  end
end
