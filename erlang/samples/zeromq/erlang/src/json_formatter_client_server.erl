-module(json_formatter_client_server).
-behavior(gen_server).

-export([ init/1, handle_call/3, terminate/2, code_change/3, handle_cast/2, handle_info/2 ]).
-export([ start_link/1, prettify/1 ]).

start_link(Ctx) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Ctx, []).

prettify(JSON) ->
    "+" ++ FormattedJSON = gen_server:call(?MODULE, {prettify, JSON}),
    io:format("~s~n", [ FormattedJSON ]).

init(Ctx) ->
    Req = czmq:zsocket_new(Ctx, req),
    ok = czmq:zsocket_connect(Req, "tcp://127.0.0.1:5555"),

    {ok, Req}.

%% Starting simulation.

handle_call({prettify, JSON}, _From, Req) ->
    ok = czmq:zstr_send(Req, JSON),
    {ok, FormattedJSON} = czmq:zstr_recv(Req),

    {reply, FormattedJSON, Req}.

terminate(_, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
