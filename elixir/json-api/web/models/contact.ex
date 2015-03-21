defmodule JsonApi.Contact do
  use Ecto.Model

  schema "contacts" do
    field :name
    field :phone

    timestamps
  end
end
