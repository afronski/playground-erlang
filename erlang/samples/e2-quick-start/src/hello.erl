-module(hello).

-behavior(e2_task).

-export([start_link/0, handle_task/1]).

start_link() ->
    e2_task:start_link(?MODULE, "Hello e2!~n").

handle_task(Msg) ->
    e2_log:info(Msg),
    { repeat, Msg, 5000 }.
