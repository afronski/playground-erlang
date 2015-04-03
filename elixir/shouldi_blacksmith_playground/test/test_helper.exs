defmodule Blacksmith.Config do
  def save(repo, map) do
    repo.insert(map)
  end

  def save_all(repo, list) do
    Enum.map(list, &repo.insert/1)
  end
end

defmodule Forge do
  use Blacksmith

  @save_one_function &Blacksmith.Config.save/2
  @save_all_function &Blacksmith.Config.save_all/2

  register :user,
    __struct__: User,
    name: "John Doe",
    email: Sequence.next(:email, &"test#{&1}@example.com")

  register :admin,
    [prototype: :user],
    roles: ["admin"]
end

ExUnit.start(formatters: [ShouldI.CLIFormatter])
