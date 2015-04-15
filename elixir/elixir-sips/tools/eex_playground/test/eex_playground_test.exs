defmodule EexPlaygroundTest do
  use ExUnit.Case

  @expected_template_output """
This is a template. It can evaluate expressions. For instance:

The sum of 1 and 2 is 3
  """

  test "rendering a template" do
    assert EEx.eval_file("test/template.eex") == @expected_template_output
  end

  test "compiling a template to a function" do
    defmodule Render do
      require EEx

      EEx.function_from_file(:def, :template, "test/template.eex")
    end

    assert Render.template == @expected_template_output
  end

  @second_template "Hey there, my name is <%= @person.name %> and I'm <%= @person.age %> years old."
  @josh %{name: "Josh", age: 31}
  @robby %{name: "Robby", age: 30}
  @expected_josh "Hey there, my name is Josh and I'm 31 years old."
  @expected_robby "Hey there, my name is Robby and I'm 30 years old."

  test "rendering a template with bound variables" do
    assert EEx.eval_string(@second_template, [assigns: [person: @josh]]) == @expected_josh
    assert EEx.eval_string(@second_template, [assigns: [person: @robby]]) == @expected_robby
  end

  test "compiling a template to a function that takes bound variables" do
    template2 = @second_template

    defmodule Render2 do
      require EEx

      EEx.function_from_string(:def, :template, template2, [:assigns])
    end

    assert Render2.template(person: @josh) == @expected_josh
    assert Render2.template(person: @robby) == @expected_robby
  end
end
