-module(consumer).

-export([ start/0, prettify/2 ]).

start() ->
	application:start(sasl),
	application:start(gen_listener_tcp),
	application:start(ezmq),

	{ok, Socket} = ezmq:start([ {type, req} ]),

	ezmq:connect(Socket, tcp, {127,0,0,1}, 5555, []),
	{ Socket }.

prettify({ Socket }, JSON) ->
    ezmq:send(Socket, [ JSON ]),
    {ok, "+" ++ FormattedJSON} = ezmq:recv(Socket),

    io:format("~s", [ FormattedJSON ]).
