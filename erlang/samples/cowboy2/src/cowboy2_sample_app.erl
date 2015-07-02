-module(cowboy2_sample_app).
-behaviour(application).

-export([ start/2, stop/1 ]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [ {"/", sample_hello_handler, []} ]}
    ]),

    {ok, _} = cowboy:start_http(sample_http_listener, 100, [ {port, 8080} ],
        [ {env, [ {dispatch, Dispatch} ]} ]
    ),

    cowboy2_sample_app_sup:start_link().

stop(_State) ->
    ok.
