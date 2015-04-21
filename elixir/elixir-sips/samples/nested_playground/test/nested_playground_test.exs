defmodule Category do
  @derive [Access]

  defstruct [:name, :projects]
end

defmodule NestedPlaygroundTest do
  use ExUnit.Case

  @category %{
    name: "Testing",
    projects: [
      amrita: %{name: "Amrita"},
      exunit: %{name: "ExUnit"}
    ]
  }

  test "getting data out of a deeply nested structure" do
    assert "Amrita" == @category.projects[:amrita].name
    assert "Amrita" == get_in(@category, [:projects, :amrita, :name])
  end

  test "adding a project to the category" do
    new_projects = Dict.put(@category.projects, :hound, %{name: "Hound"})
    new_category = %{@category | projects: new_projects}
    assert "Hound" == new_category.projects[:hound].name

    put_category = put_in(@category, [:projects, :hound], %{name: "Hound"})
    assert "Hound" == put_category.projects[:hound].name
  end

  test "updating a project's name" do
    new_projects = Dict.update!(@category.projects, :amrita, fn (proj) ->
      %{ proj | name: String.upcase(proj.name) }
    end)

    new_category = %{@category | projects: new_projects}
    assert "AMRITA" == new_category.projects[:amrita].name

    updated_category = update_in(@category, [:projects, :amrita, :name], &String.upcase/1)
    assert "AMRITA" == updated_category.projects[:amrita].name
  end

  test "fetch and update in one" do
    {original, updated} = get_and_update_in(@category, [:projects, :amrita, :name], &({&1, String.upcase(&1)}))

    assert "AMRITA" == updated.projects[:amrita].name
    assert "Amrita" == original
  end

  test "get_in a struct" do
    struct = %Category{
      name: "Testing",
      projects: [
        amrita: %{name: "Amrita"},
        exunit: %{name: "ExUnit"}
      ]
    }

    assert "Amrita" == get_in(struct, [:projects, :amrita, :name])
  end
end
