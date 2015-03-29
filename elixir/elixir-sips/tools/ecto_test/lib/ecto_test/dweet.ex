defmodule EctoTest.Dweet do
  use Ecto.Model

  schema "dweets" do
    field :content, :string
    field :author, :string
  end
end
