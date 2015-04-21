defmodule ExprotobufPlaygroundTest do
  use ExUnit.Case

  alias ExprotobufPlayground.Data.Person
  alias ExprotobufPlayground.Data.Person.PhoneNumber
  alias ExprotobufPlayground.Data.AddressBook

  test "can define structs from the proto file" do
    assert %Person{name: "Josh"}.name == "Josh"
  end

  test "building an addressbook" do
    addressbook = %AddressBook{
      person: [
        %Person{name: "Josh Adams", id: 1, phone: [
            %PhoneNumber{number: "2055551212"}
          ]
        },
        %Person{name: "Jose Valim", id: 2}
      ]
    }

    binary = Protobuf.Encoder.encode(addressbook, AddressBook.defs)
    addressbook2 = Protobuf.Decoder.decode(binary, AddressBook)

    josh = hd(addressbook2.person)

    assert josh.name == "Josh Adams"
    assert hd(josh.phone).number == "2055551212"
  end
end
