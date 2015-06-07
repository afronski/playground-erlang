defmodule User do
  use Ecto.Model

  schema "users" do
    field :name, :string
    field :email, :string
  end
end
