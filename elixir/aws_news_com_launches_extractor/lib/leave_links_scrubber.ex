defmodule LeaveLinksScrubber do
  require HtmlSanitizeEx.Scrubber.Meta
  alias HtmlSanitizeEx.Scrubber.Meta

  Meta.remove_cdata_sections_before_scrub()
  Meta.strip_comments()

  Meta.allow_tag_with_these_attributes("a", ["href"])

  Meta.strip_everything_not_covered()
end
