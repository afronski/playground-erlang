defmodule VirtualPetMacros do
  defmacro update_action(name) do
    function = String.to_atom("#{name}_action")

    quote do
      def unquote(name)(name), do: update(name, unquote(name))

      defp update(name, unquote(name)) do
        Agent.update(name, fn state -> unquote(function)(state) end)

        show(name)
        step(name)
      end
    end
  end
end

defmodule VirtualPet do
  require VirtualPetMacros

  defstruct name: "", age: 0, hunger: 0, dirtyness: 0, thirst: 0, sadness: 0

  def new(name) when is_atom(name) do
    Agent.start_link(fn -> %VirtualPet{ name: name } end, name: name)

    writeln("#{name} was born!");
    show(name)

    name
  end

  def show(name), do: get(name, :show)

  defp get(name, :show) do
    Agent.get(name, fn state -> nice_state(state) end)
    step(name)
  end

  VirtualPetMacros.update_action :play
  VirtualPetMacros.update_action :feed
  VirtualPetMacros.update_action :clean
  VirtualPetMacros.update_action :drink

  defp play_action(state), do: %{ state | sadness: state.sadness - 18 }
  defp feed_action(state), do: %{ state | hunger: state.hunger - 30 }
  defp clean_action(state), do: %{ state | dirtyness: state.dirtyness - 20 }
  defp drink_action(state), do: %{ state | thirst: state.thirst - 25 }

  defp step(name) do
    Agent.update(name, fn state -> %{ state |
      age: state.age + 1,
      hunger: state.hunger + 1,
      dirtyness: state.dirtyness + 1,
      thirst: state.thirst + 1,
      sadness: state.sadness + 1
    } end)
  end

  defp nice_state(%VirtualPet{ name: name, sadness: sadness }) when sadness > 60, do: writeln("#{name} is crying.")
  defp nice_state(%VirtualPet{ name: name, sadness: sadness }) when sadness > 40, do: writeln("#{name} is sad.")
  defp nice_state(%VirtualPet{ name: name, sadness: sadness }) when sadness > 20, do: writeln("#{name} is upset.")
  defp nice_state(%VirtualPet{ name: name, sadness: sadness }) when sadness < -20, do: writeln("#{name} is extatic.")
  defp nice_state(%VirtualPet{ name: name, sadness: sadness }) when sadness < 0, do: writeln("#{name} is happy.")

  defp nice_state(%VirtualPet{ name: name, dirtyness: dirtyness }) when dirtyness > 0, do: writeln("#{name} is dirty.")
  defp nice_state(%VirtualPet{ name: name, dirtyness: dirtyness }) when dirtyness <= 0, do: writeln("#{name} is clean.")

  defp nice_state(%VirtualPet{ name: name, thirst: thirst }) when thirst > 35, do: writeln("#{name} is parched.")
  defp nice_state(%VirtualPet{ name: name, thirst: thirst }) when thirst > 15, do: writeln("#{name} is thirsty.")

  defp nice_state(%VirtualPet{ name: name, hunger: hunger }) when hunger > 100, do: writeln("#{name} is starving.")
  defp nice_state(%VirtualPet{ name: name, hunger: hunger }) when hunger > 50, do: writeln("#{name} is hungry.")
  defp nice_state(%VirtualPet{ name: name, hunger: hunger }) when hunger > 25, do: writeln("#{name} is peckish.")
  defp nice_state(%VirtualPet{ name: name, hunger: hunger }) when hunger > 0, do: writeln("#{name} is full.")

  defp nice_state(%VirtualPet{ name: name, age: age }) when age < 50, do: writeln("#{name} is a baby.")
  defp nice_state(%VirtualPet{ name: name, age: age }) when age < 250, do: writeln("#{name} is a child.")
  defp nice_state(%VirtualPet{ name: name, age: age }) when age < 450, do: writeln("#{name} is a teenager.")
  defp nice_state(%VirtualPet{ name: name, age: age }) when age < 750, do: writeln("#{name} is an adult.")
  defp nice_state(%VirtualPet{ name: name, age: age }) when age < 950, do: writeln("#{name} is getting old.")

  defp nice_state(%VirtualPet{ name: name }), do: writeln("#{name} is fine.")

  defp writeln(text) do
    IO.puts text
  end
end