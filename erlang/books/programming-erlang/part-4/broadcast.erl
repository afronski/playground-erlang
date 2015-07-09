-module(broadcast).
-export([ send/1, listen/0, loop/0 ]).

send(IoList) ->
    case inet:ifget("eth0", [ broadaddr ]) of
        {ok, [ {broadaddr, Ip} ]} ->
            {ok, S} =  gen_udp:open(5010, [ {broadcast, true} ]),
            gen_udp:send(S, Ip, 6000, IoList),
            gen_udp:close(S);

        _ ->
            io:format("Bad interface name, or\nbroadcasting not supported\n")
    end.

listen() ->
    {ok, _} = gen_udp:open(6000),
    loop().

loop() ->
    receive
        Any ->
            io:format("received:~p~n", [ Any ]),
            loop()
    end.
