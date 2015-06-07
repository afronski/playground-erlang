defmodule JsonApi.ContactControllerTest do
  use JsonApi.Case, async: false

  test "/index returns a list of contacts" do
    contacts_as_json =
      %Contact{name: "Foo", phone: "(801) 555-5555"}
      |> Repo.insert
      |> List.wrap
      |> Poison.encode!

    response = conn(:get, "/api/contacts") |> send_request

    assert response.status == 200
    assert response.resp_body == contacts_as_json
  end
end
