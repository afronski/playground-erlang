defmodule EctoTestTest do
  use ExUnit.Case
  import Ecto.Query, only: [ from: 2 ]

  setup do
    EctoTest.Repo.start_link

    on_exit fn ->
      EctoTest.Repo.start_link

      query = from dweet in EctoTest.Dweet,
              where: dweet.author == "afronski"

      EctoTest.Repo.delete_all query
    end

    :ok
  end

  test "created dweet can be successfully stored in database" do
    dweet = %EctoTest.Dweet{content: "foo", author: "afronski"}
    EctoTest.Repo.insert dweet

    query = from dweet in EctoTest.Dweet,
            where: dweet.author == "afronski",
            select: dweet

    assert Enum.count(EctoTest.Repo.all(query)) == 1
  end
end
