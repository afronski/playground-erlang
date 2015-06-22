defmodule AgendaTest do
  use ExUnit.Case

  @command1 Code.string_to_quoted!("Module.function(:arg1)")
  @command2 Code.string_to_quoted!("Module.function(:arg2, :arg3)")

  test "parse a schedule string" do
    assert Agenda.Parser.parse("0 0 0 0 0 Module.function(:arg1)") == %Agenda.Schedule{minute: [0], hour: [0], day_of_month: [0], month: [0], day_of_week: [0], command: @command1}
    assert Agenda.Parser.parse("1 0 0 0 0 Module.function(:arg2, :arg3)") == %Agenda.Schedule{minute: [1], hour: [0], day_of_month: [0], month: [0], day_of_week: [0], command: @command2}
  end

  test "parse multiple hours in the patterns" do
    assert Agenda.Parser.parse("0 1,2 0 0 0 Module.function(:arg1)") == %Agenda.Schedule{minute: [0], hour: [1,2], day_of_month: [0], month: [0], day_of_week: [0], command: @command1}
  end

  test "parsing a wildcard" do
    assert Agenda.Parser.parse("0 0 0 0 * Module.function(:arg1)") == %Agenda.Schedule{minute: [0], hour: [0], day_of_month: [0], month: [0], day_of_week: [0,1,2,3,4,5,6], command: @command1}
  end

  test "parsing patterns like 'every five minutes'" do
    assert Agenda.Parser.parse("*/5 0 0 0 0 Module.function(:arg1)") == %Agenda.Schedule{minute: [0,5,10,15,20,25,30,35,40,45,50,55], hour: [0], day_of_month: [0], month: [0], day_of_week: [0], command: @command1}
  end
end
