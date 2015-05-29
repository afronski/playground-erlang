-module(json_formatter_client_sup).
-behaviour(supervisor).

-export([ init/1 ]).
-export([ start_link/1 ]).

start_link(Ctx) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Ctx).

init(Ctx) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 5000,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Child = {json_formatter_client_server,
             {json_formatter_client_server, start_link, [ Ctx ]},
             Restart, Shutdown, Type,
             [ json_formatter_client_server ]},

    {ok, {SupFlags, [ Child ]}}.
