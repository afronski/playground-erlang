-module(json_formatter_client_app).
-behavior(application).

-export([ start/2, stop/1 ]).

start(_Type, _Args) ->
    {ok, Ctx} = czmq:start_link(),
    json_formatter_client_sup:start_link(Ctx).

stop(_State) ->
    ok.
