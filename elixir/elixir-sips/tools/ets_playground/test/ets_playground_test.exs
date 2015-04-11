defmodule EtsPlaygroundTest do
  use ExUnit.Case

  setup do
    _cars = :ets.new(:cars, [:bag, :named_table])

    :cars |> :ets.insert({"328i", "BMW", "White", 2011})
    :cars |> :ets.insert({"335i", "BMW", "White", 2013})
    :cars |> :ets.insert({"528i", "BMW", "White", 2012})

    # ETS table will be auto-deleted when test ends.
    :ok
  end

  test "creating a table and getting its info" do
    info = :ets.info(:cars)
    IO.inspect info

    assert info[:type] == :bag
  end

  test "inserting and retrieving data" do
    [{_model, make, _color, _year} | _tail] = :ets.lookup(:cars, "328i")
    assert make == "BMW"
  end

  test "traversing the table sequentially" do
    first = :ets.first(:cars)
    second = :ets.next(:cars, first)
    third = :ets.next(:cars, second)

    assert third == "528i"
    assert :"$end_of_table" == :ets.next(:cars, third)
  end

  test "querying the table table for data that matches a pattern" do
    query = {:_, :_, :_, 2012}

    cars_from_2012 = :ets.match_object(:cars, query)
    [{model, _, _, _} | _tail] = cars_from_2012

    assert model == "528i"
  end

  test "querying using the match specs" do
    query = [
      {
          # Pattern which should match, with variables extraction:
          {:_, :_, :_, :"$1"},

          # Guard clauses (filtering):
          [{:andalso,
            {:">=", :"$1", 2011},
            {:"=<", :"$1", 2012}
           }],

          # What to return (`:"$_"`` means full object):
          [:"$_"]
      }
    ]

    cars_between_2011_2012 = :ets.select(:cars, query)

    IO.inspect cars_between_2011_2012

    assert Enum.count(cars_between_2011_2012) == 2
  end
end
