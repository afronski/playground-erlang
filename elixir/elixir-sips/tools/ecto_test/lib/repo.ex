defmodule EctoTest.Repo do
  use Ecto.Repo, otp_app: :ecto_test

  def url do
    "ecto://postgres:postgres@localhost/ecto_test"
  end
end
