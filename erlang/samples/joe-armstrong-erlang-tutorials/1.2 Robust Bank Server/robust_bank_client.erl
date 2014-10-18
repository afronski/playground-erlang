-module(robust_bank_client).

-export([ deposit/2, withdraw/2, balance/1 ]).

deposit(Who, X) ->
    robust_rpc({ deposit, Who, X }).

withdraw(Who, X) ->
    robust_rpc({ withdraw, Who, X }).

balance(Who) ->
    robust_rpc({ balance, Who }).

robust_rpc(X) ->
    Id = make_ref(),
    X1 = { call, Id, X },

    io:format("Trying to connect to server 1.~n"),

    case gen_tcp:connect("localhost", 3020,
                         [ binary, { packet, 4 } ]) of
    { ok, Socket } ->
        io:format("Sending to server 1.~n"),
        gen_tcp:send(Socket, [ term_to_binary(X1) ]),
        wait_reply1(Socket, Id, X);

    { error, _ } ->
        io:format("Cannot connect to server 1.~n"),
        robust_rpc_try_again(Id, X)
    end.

wait_reply1(Socket, Id, X) ->
    receive
        { tcp, Socket, Bin } ->
            case binary_to_term(Bin) of
                { ack, Id, Reply } ->
                    io:format("Server 1 replied.~n"),
                    B = term_to_binary({delete_tag, Id}),
                    gen_tcp:send(Socket, B),
                    gen_tcp:close(Socket),
                    { ok, { server1, Reply } };
        _ ->
            robust_rpc_try_again(Id, X)
        end;

        { tcp_closed, Socket } ->
            robust_rpc_try_again(Id, X)

    after 10000 ->
        io:format("Timeout from server 1.~n"),
        gen_tcp:close(Socket),
        robust_rpc_try_again(Id, X)
    end.

robust_rpc_try_again(Id, X) ->
    io:format("Trying to connect to server 2.~n"),
    case gen_tcp:connect("localhost", 3030,
                         [ binary, { packet, 4 } ]) of
        { ok, Socket } ->
            X1 = { call, Id, X },
            io:format("Sending to server 2.~n"),
            gen_tcp:send(Socket, [ term_to_binary(X1) ]),
            wait_reply2(Socket, Id);

        { error, _ } ->
            io:format("Cannot connect to server 2.~n"),
            { error, both_servers_down }
    end.

wait_reply2(Socket, Id) ->
    receive
        { tcp, Socket, Bin } ->
            case binary_to_term(Bin) of
                { ack, Id, Reply } ->
                    B =  term_to_binary({ delete_tag, Id }),
                    gen_tcp:send(Socket, B),
                    gen_tcp:close(Socket),
                    { ok, { server2, Reply } };
            O ->
                { error, { unexpect_reply, O } }
        end;

        { tcp_closed, Socket } ->
            { error, server2 }

    after 10000 ->
        io:format("Timeout from server 2.~n"),
        gen_tcp:close(Socket),
        { error, no_reply_sever2 }
    end.