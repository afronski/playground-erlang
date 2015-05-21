defmodule TextParsing.Parser do
  defmodule Parsed do
    defstruct document_number: "", text: ""
  end

  defstruct page_number: 0,
            column_number: 0,
            line_number: 0,
            y1: 0,
            y2: 0

  def parse(content) do
    parse(String.split(content, "\n"), %Parsed{}, %TextParsing.Parser{})
  end

  defp parse([ "Word Positions for " <> doc_number_and_details | rest ], parsed, parser) do
    doc_number = doc_number_and_details
                 |> String.split
                 |> Enum.take(2)
                 |> Enum.join(" ")

    parse(rest, %Parsed{ parsed | document_number: doc_number }, parser)
  end

  defp parse([ "Page: " <> page_number_and_metadata | rest ], parsed, parser) do
    page_number = page_number_and_metadata
                  |> String.split
                  |> Enum.take(1)

    parse(rest, parsed, %TextParsing.Parser{ parser | page_number: page_number })
  end

  defp parse([ "Column: " <> column_number_and_newline | rest ], parsed, parser) do
    column_number = column_number_and_newline
                    |> String.split
                    |> Enum.take(1)

    parse(rest, parsed, %TextParsing.Parser{ parser | column_number: column_number })
  end

  defp parse([ "Line: " <> line_number_and_metadata | rest ], parsed, parser) do
    [ line_number, y1, y2, _, _ ] = line_number_and_metadata
                                    |> String.split

    parse(rest, parsed, %TextParsing.Parser{
      parser | line_number: line_number,
               y1: y1,
               y2: y2
    })
  end

  defp parse([ "\t" <> x1_x2_and_word | rest ], parsed, parser) do
    [ x1, x2, word ] = String.split(x1_x2_and_word)
    word_and_metadata = "#{word}=#{parser.page_number},#{parser.column_number},#{parser.line_number},#{x1},#{x2},#{parser.y1},#{parser.y2} "

    parse(rest, %Parsed{ parsed | text: parsed.text <> word_and_metadata }, parser)
  end

  defp parse([ _line | rest ], parsed, parser) do
    parse(rest, parsed, parser)
  end

  defp parse([], parsed, _parser) do
    parsed
  end
end
