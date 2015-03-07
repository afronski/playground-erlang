-module(e2test).

-export([start/0, stop/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

start() ->
    e2_application:start_with_dependencies(e2test).

stop() ->
    application:stop(e2test).
