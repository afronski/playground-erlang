defmodule EctoTestTest do
  use ExUnit.Case
  import Ecto.Query, only: [ from: 2 ]

  defp test_dweets do
    from dweet in EctoTest.Dweet,
         where: dweet.author == "afronski"
  end

  setup do
    on_exit fn ->
      EctoTest.Repo.delete_all test_dweets
    end

    :ok
  end

  test "created Dweet can be successfully stored in database" do
    dweet = %EctoTest.Dweet{content: "foo", author: "afronski"}
    EctoTest.Repo.insert dweet

    assert Enum.count(EctoTest.Repo.all test_dweets) == 1
  end
end
