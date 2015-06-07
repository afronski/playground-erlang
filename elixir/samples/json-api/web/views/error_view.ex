defmodule JsonApi.ErrorView do
  use JsonApi.Web, :view

  def render("404", _assigns) do
    "Page not found!"
  end

  def render("500", _assigns) do
    "Server internal error!"
  end

  def template_not_found(_, assigns) do
    render "404", assigns
  end
end
