defmodule WebassemblyPlayground do
  use WebAssembly

  def a_list(things) do
    builder do
      ul do
        for thing <- things do
          li thing
        end
      end
    end
  end

  def module_functions(module) do
    builder do
      h2 module.__info__(:module)
      ul do
        for {name, arity} <- module.__info__(:functions) do
          li do
            strong name
            em arity
          end
        end
      end
    end
  end
end
