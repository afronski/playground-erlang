defmodule Log do
  use Elixiak.Model

  document "log" do
    field :application, :string, indexed: true
    field :time, :datetime, indexed: true
    field :content, :binary
  end
end
