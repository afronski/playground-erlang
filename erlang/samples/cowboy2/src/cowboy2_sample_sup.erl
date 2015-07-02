-module(cowboy2_sample_sup).
-behaviour(supervisor).

-export([ init/1 ]).
-export([ start_link/0 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 5000,

    Flags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {Flags, []}}.
