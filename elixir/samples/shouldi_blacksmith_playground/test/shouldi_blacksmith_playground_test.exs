defmodule ShouldiBlacksmithPlaygroundTest do
  use ShouldI

  # NOTE: Fix is not pushed to the Hex :(.
  use ExUnit.Case

  should "be ok" do
    assert :ok == :ok
  end

  with "context" do
    setup context do
      Repo.start_link
      Enum.into([ user: Forge.saved_user(Repo), admin: Forge.admin ], context)
    end

    should "has context passed from setup", context do
      assert context.user.email == "test0@example.com"
      assert context.admin.email == "test1@example.com"
    end
  end
end
