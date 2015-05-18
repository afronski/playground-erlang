defmodule DockerPlaygroundTest do
  use ExUnit.Case

  test "list all docker images" do
    {:ok, images} = :docker_image.images
    IO.inspect(images)
  end

  test "get image details and history" do
    {:ok, images} = :docker_image.images
    image = hd(images)
    id = image[:Id]

    IO.inspect(:docker_image.image(id))
    IO.inspect(:docker_image.history(id))
  end

  test "search image in a docker remote registry by a term" do
    IO.inspect(:docker_image.search("elixir"))
  end
end
