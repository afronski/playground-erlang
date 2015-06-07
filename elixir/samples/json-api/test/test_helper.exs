defmodule JsonApi.Case do
  use ExUnit.CaseTemplate

  alias Ecto.Adapters.SQL
  alias JsonApi.Repo

  setup do
    SQL.begin_test_transaction(Repo)

    on_exit fn ->
      SQL.rollback_test_transaction(Repo)
    end
  end

  using do
    quote do
      alias JsonApi.Repo
      alias JsonApi.Contact

      use Plug.Test

      def send_request(conn) do
        conn
        |> put_private(:plug_skip_csrf_protection, true)
        |> JsonApi.Endpoint.call([])
      end
    end
  end
end

ExUnit.start
