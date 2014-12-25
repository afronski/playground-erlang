use Mix.Config

config :iex, default_prompt: ">>>"

config :kv, :routing_table, [ { ?a..?m, :"foo@GrayBox" },
                              { ?n..?z, :"bar@GrayBox" } ]