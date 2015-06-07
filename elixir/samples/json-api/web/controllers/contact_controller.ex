defmodule JsonApi.ContactController do
  use JsonApi.Web, :controller

  alias JsonApi.Repo
  alias JsonApi.Contact

  plug :action

  def index(conn, _params) do
    contacts = Repo.all(Contact)
    render conn, contacts: contacts
  end
end
