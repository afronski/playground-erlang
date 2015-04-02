defmodule IPPacket do
  def from_bits(bits) do
    << protocol_version :: size(4),
       header_length_in_words :: size(4),
       _type_of_service_legacy :: size(4),
       type_of_service_int :: size(4),
       total_length_in_bytes :: size(16),
       id :: size(16),
       _flags :: size(3),
       fragmentation_offset :: size(13),
       ttl :: size(8),
       protocol :: size(8),
       header_checksum :: size(16),
       source_ip_address :: size(32),
       destination_ip_address :: size(32),
       options :: size(32),
       data :: binary >> = bits

    %{ :protocol_version => protocol_version,
       :header_length_in_bytes => header_length_in_words * (32 / 8),
       :type_of_service => type_of_service_for(type_of_service_int),
       :total_length_in_bytes => total_length_in_bytes,
       :id => id,
       :fragmentation_offset => fragmentation_offset,
       :ttl => ttl,
       :protocol => recognize_protocol(protocol),
       :header_checksum => header_checksum,
       :source_ip => stringify_ip(source_ip_address),
       :destination_ip => stringify_ip(destination_ip_address),
       :options => options,
       :data => data
     }
  end

  defp stringify_ip(ip) do
    << first, second, third, fourth >> = << ip :: size(32) >>
    "#{first}.#{second}.#{third}.#{fourth}"
  end

  defp recognize_protocol(protocol) do
    case protocol do
      17 -> :udp
      6 -> :tcp
      2 -> :igmp
      1 -> :icmp
      _ -> :unknown
    end
  end

  defp type_of_service_for(type_of_service_int) do
    case type_of_service_int do
      8 -> :minimize_delay
      4 -> :maximize_troughput
      2 -> :maximize_reliability
      1 -> :minimize_monetary_cost
      0 -> :unspecified
    end
  end
end
