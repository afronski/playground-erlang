defmodule DetsPlaygroundTest do
  use ExUnit.Case

  @dets_file "cars.dets"

  setup do
    :ets.new(:ets_cars, [:set, :named_table])

    :ets_cars |> :ets.insert({"328i", "BMW", "White", 2011, 5})
    :ets_cars |> :ets.insert({"335i", "BMW", "White", 2013, 6})
    :ets_cars |> :ets.insert({"528i", "BMW", "White", 2012, 4})

    {:ok, _} = :dets.open_file(:cars, [file: @dets_file, type: :set])

    # Easy way to persist existing in-memory ETS table:
    :dets.from_ets(:cars, :ets_cars)

    on_exit fn ->
      :dets.close(:cars)
      File.rm(@dets_file)

      :ok
    end

    :ok
  end

  test "creating a table and getting its info" do
    info = :dets.info(:cars)
    IO.inspect info

    assert info[:type] == :set
  end

  test "inserting and retrieving data" do
    [{_model, make, _color, _year, _count} | _tail] = :dets.lookup(:cars, "328i")
    assert make == "BMW"
  end

  test "traversing the table sequentially" do
    first = :dets.first(:cars)
    second = :dets.next(:cars, first)
    third = :dets.next(:cars, second)

    assert third == "528i"
    assert :"$end_of_table" == :dets.next(:cars, third)
  end

  test "querying the table table for data that matches a pattern" do
    query = {:_, :_, :_, 2012, :_}

    cars_from_2012 = :dets.match_object(:cars, query)
    [{model, _, _, _, _} | _tail] = cars_from_2012

    assert model == "528i"
  end

  test "querying using the match specs" do
    query = [
      {
          # Pattern which should match, with variables extraction:
          {:_, :_, :_, :"$1", :_},

          # Guard clauses (filtering):
          [{:andalso,
            {:">=", :"$1", 2011},
            {:"=<", :"$1", 2012}
           }],

          # What to return (`:"$_"`` means full object):
          [:"$_"]
      }
    ]

    cars_between_2011_2012 = :dets.select(:cars, query)

    IO.inspect cars_between_2011_2012

    assert Enum.count(cars_between_2011_2012) == 2
  end

  test "updating a counter atomically" do
    [{_, _, _, _, count} | _rest] = :dets.lookup(:cars, "328i")
    assert count == 5

    # At 5th position update value by '3':
    :dets.update_counter(:cars, "328i", {5, 3})

    [{_, _, _, _, count} | _rest] = :dets.lookup(:cars, "328i")
    assert count == 8
  end
end
