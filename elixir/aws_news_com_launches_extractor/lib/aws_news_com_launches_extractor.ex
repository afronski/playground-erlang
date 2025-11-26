defmodule AwsNewsComLaunchesExtractor do
  alias HtmlSanitizeEx.Scrubber

  @feed_url "https://aws.amazon.com/about-aws/whats-new/recent/feed/"

  def get_pre_reinvent_launches(cutoff_date \\ ~D[2025-11-11]) do
    {:ok, parsed_rss} = Req.get!(@feed_url).body |> FastRSS.parse_rss()

    parsed_rss
    |> Map.get("items")
    |> Enum.filter(&Date.after?(Timex.parse!(&1["pub_date"], "{RFC1123}"), cutoff_date))
    |> Enum.map(&remap_fields/1)
    |> Enum.sort(&(DateTime.compare(&1["Announcement Date"], &2["Announcement Date"]) != :gt))
  end

  def save_to_csv(entries, filename \\ "aws-whats-new-riv25-launches.csv") do
    file = File.open!(filename, [:write, :utf8])

    entries
    |> CSV.encode(
      headers: [
        "Unique Identifier",
        "Announcement Type",
        "Announcement Title",
        "Category",
        "AWS Service",
        "Notes",
        "Announcement Date",
        "Announcement Summary",
        "AWS What's New URL",
        "AWS News Blog URL",
        "Available Regions",
        "Included As",
        "Priority Announcement",
        "Prepare Video",
        "Prepare Code Sample",
        "Slide Count",
        "Slides",
        "Accepted Slides",
        "In Progress Slides",
        "Completion Rate (%)"
      ]
    )
    |> Enum.each(&IO.write(file, &1))
  end

  defp extract_hrefs({_node, attributes, _children}) do
    attributes
    |> Enum.filter(fn({attribute_name, _}) -> attribute_name == "href" end)
    |> Enum.map(fn({_, attribute_value}) -> attribute_value end)
  end

  defp remap_fields(announcement) do
    categories = announcement["categories"] |> Enum.at(0) |> Map.get("name")
    today = DateTime.utc_now() |> DateTime.to_iso8601()

    only_links = Scrubber.scrub(announcement["description"], LeaveLinksScrubber)
    just_text = Scrubber.scrub(only_links, JustTextScrubber)

    {:ok, document} = Floki.parse_document(only_links)
    links = 
      Floki.find(document, "a") 
      |> Enum.map(&extract_hrefs/1) 

    summary = """
    #{just_text}
    ---
    #{Enum.join(links, "\n")}
    """

    %{
      "Unique Identifier" => announcement["guid"]["value"],
      "Announcement Type" => "",
      "Announcement Title" => announcement["title"],
      "Category" => categories,
      "Secondary Categories" => "",
      "AWS Service" => categories,
      "Notes" => "Extracted from AWS What's New RSS Feed (extracted: #{today}).",
      "Announcement Date" => Timex.parse!(announcement["pub_date"], "{RFC1123}"),
      "Announcement Summary" => summary,
      "AWS What's New URL" => announcement["link"],
      "AWS News Blog URL" => "",
      "Other URLs" => "",
      "Available Regions" => "",
      "Included As" => "To Review",
      "Priority Announcement" => false,
      "Prepare Video" => false,
      "Prepare Code Sample" => false,
      "Slide Count" => 0,
      "Slides" => "",
      "Accepted Slides" => 0,
      "In Progress Slides" => 0,
      "Completion Rate (%)" => 0
    }
  end
end
