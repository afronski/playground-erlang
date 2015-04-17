defmodule ExprotobufPlayground.Data do
  use Protobuf, from: Path.expand("../../test/files/addressbook.proto", __DIR__)
end
