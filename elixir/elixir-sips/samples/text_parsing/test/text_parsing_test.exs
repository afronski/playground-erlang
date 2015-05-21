defmodule TextParsingTest do
  use ExUnit.Case

  @file File.read!("./test/examples/sample.txt")
  @result TextParsing.Parser.parse(@file)

  test "extracting the document number" do
    assert "Doc 123" == @result.document_number
  end

  test "extracts words with metadata attached" do
    assert String.contains?(@result.text, "WHEN=1,1,1,2288,2920,1412,1512")
  end
end
