defmodule WebsitePipelineTest do
  use ExUnit.Case

  test "Mapping a pipeline of websites" do
    sites = [
      "http://example.org",
      "http://slashdot.org",
      "http://elixir-lang.org",
      "http://www.erlang.org"
    ]

    expected_titles = [
      "Example Domain",
      "Slashdot: News for nerds, stuff that matters",
      "Elixir",
      "Erlang Programming Language"
    ]

    assert expected_titles == WebsitePipeline.map_titles(sites)
  end
end
