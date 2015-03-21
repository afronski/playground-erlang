defmodule JsonApi.ContactView do
  use JsonApi.Web, :view

  def render("index.json", %{contacts: contacts}) do
    contacts
  end
end
