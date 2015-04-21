defmodule EctoTest.Repo.Migrations.CreateDweets do
  use Ecto.Migration

  def change do
    create table(:dweets) do
      add :content, :string
      add :author, :string
    end
  end
end
