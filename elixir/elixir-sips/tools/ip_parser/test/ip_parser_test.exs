defmodule IpParserTest do
  use ExUnit.Case

  setup do
    bits = File.read!("./packet.bits")
    packet = IPPacket.from_bits(bits)

    {:ok, packet: packet}
  end

  test "getting protocol version", meta do
    assert meta[:packet][:protocol_version] == 4
  end

  test "getting header length in bytes", meta do
    assert meta[:packet][:header_length_in_bytes] == 20
  end

  test "getting type of service", meta do
    assert meta[:packet][:type_of_service] == :unspecified
  end

  test "getting total length in bytes", meta do
    assert meta[:packet][:total_length_in_bytes] == 44
  end

  test "getting ID of a packet", meta do
    assert meta[:packet][:id] == 9394
  end

  test "getting fragmentation offset", meta do
    assert meta[:packet][:fragmentation_offset] == 0
  end

  test "getting TTL", meta do
    assert meta[:packet][:ttl] == 64
  end

  test "getting packet protocol", meta do
    assert meta[:packet][:protocol] == :tcp
  end

  test "getting header checksum", meta do
    assert meta[:packet][:header_checksum] == 64991
  end

  test "getting source IP as a string", meta do
    assert meta[:packet][:source_ip] == "172.16.0.9"
  end

  test "getting destination IP as a string", meta do
    assert meta[:packet][:destination_ip] == "172.16.0.1"
  end
end
