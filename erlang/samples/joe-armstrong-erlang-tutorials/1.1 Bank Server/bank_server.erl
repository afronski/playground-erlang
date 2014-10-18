-module(bank_server).

-export([ start/0, stop/0 ]).

start() ->
    mnesia:start(),
    spawn_link(fun() -> server(3010) end).

stop() ->
    mnesia:stop(),
    tcp_server:stop(3010).

server(Port) ->
    tcp_server:start_raw_server(
        Port,
        fun(Socket) -> input_handler(Socket) end,
        15,
        4).

input_handler(Socket) ->
    receive
        { tcp, Socket, Bin } ->
            Term = binary_to_term(Bin),
            Reply = do_call(Term),
            send_term(Socket, Reply),
            input_handler(Socket);

        { tcp_closed, Socket } ->
            true
    end.

send_term(Socket, Term) ->
    gen_tcp:send(Socket, [ term_to_binary(Term) ]).

do_call(C) ->
    Fun = the_func(C),
    mnesia:transaction(Fun).

the_func({ deposit, Who, X }) ->
    bank:deposit(Who, X);
the_func({ withdraw, Who, X }) ->
    bank:withdraw(Who, X);
the_func({ balance, Who }) ->
    bank:balance(Who).