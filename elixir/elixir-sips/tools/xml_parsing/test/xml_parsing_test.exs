defmodule XmlParsingTest do
  use ExUnit.Case

  def sample_xml do
    """
    <html>
      <head>
        <title>XML parsing</title>
      </head>
      <body>
        <p>Neato</p>
        <ul>
          <li>First</li>
          <li>Second</li>
        </ul>
      </body>
    </html>
    """
  end

  defp getValue({:xmlText, _, _, _, value, _}), do: value

  defp getElement([ {:xmlElement, _, _, _, _, _, _, _, [ element ], _, _, _} ]), do: element

  test "parsing the title out" do
    { xml, _rest } = :xmerl_scan.string(:erlang.bitstring_to_list(sample_xml))
    title_element = getElement(:xmerl_xpath.string('/html/head/title', xml))

    assert getValue(title_element) == 'XML parsing'
  end

  test "parsing the <p> tag" do
    { xml, _rest } = :xmerl_scan.string(:erlang.bitstring_to_list(sample_xml))
    [ p_text_node ] = :xmerl_xpath.string('/html/body/p/text()', xml)

    assert getValue(p_text_node) == 'Neato'
  end

  test "parsing the <li> tags and map them" do
    { xml, _rest } = :xmerl_scan.string(:erlang.bitstring_to_list(sample_xml))
    li_text_nodes = :xmerl_xpath.string('/html/body/ul/li/text()', xml)
    texts = li_text_nodes |> Enum.map(&getValue/1)

    assert texts == ['First', 'Second']
  end
end
