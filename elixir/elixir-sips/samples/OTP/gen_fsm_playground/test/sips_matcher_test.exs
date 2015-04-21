defmodule SipsMatcherTest do
  use ExUnit.Case

  test "[:starting] it successfully consume the string 's'" do
    fsm = SipsMatcher.start_link
    assert SipsMatcher.consume_s(fsm) == :got_s
  end

  test "[:starting] it successfully consume strings other than 's'" do
    fsm = SipsMatcher.start_link
    assert SipsMatcher.consume_not_s(fsm) == :starting
  end

  test "it successfully consumes the string 'sips'" do
    fsm = SipsMatcher.start_link

    SipsMatcher.consume_s(fsm)
    SipsMatcher.consume_i(fsm)
    SipsMatcher.consume_p(fsm)

    assert SipsMatcher.consume_s(fsm) == :got_sips
  end

  test "it successfully consumes strings without a match" do
    fsm = SipsMatcher.start_link

    SipsMatcher.consume_s(fsm)
    SipsMatcher.consume_i(fsm)
    SipsMatcher.consume_p(fsm)

    assert SipsMatcher.consume_not_s(fsm) == :starting
  end

  test "it successfully consumes any string after consuming 'sips'" do
    fsm = SipsMatcher.start_link

    SipsMatcher.consume_s(fsm)
    SipsMatcher.consume_i(fsm)
    SipsMatcher.consume_p(fsm)
    SipsMatcher.consume_s(fsm)

    assert SipsMatcher.consume_not_s(fsm) == :got_sips
    assert SipsMatcher.consume_i(fsm) == :got_sips
  end
end
