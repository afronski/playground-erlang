defmodule Mix.Tasks.Scream do
  use Mix.Task

  @shortdoc "Scream the input"

  @moduledoc """
  Given a particular input, scream it.
  """

  def run(args) do
    {_opts, args, _} = OptionParser.parse(args)
    string = Enum.join(args, " ")
    Mix.shell.info(string |> String.upcase)
  end
end
