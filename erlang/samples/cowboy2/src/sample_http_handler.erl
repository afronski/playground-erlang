-module(sample_http_handler).

-export([ init/2 ]).

init(Req, Opts) ->
    Headers = [ {<<"content-type">>, <<"text/plain">>} ],
    OutputReq = cowboy_req:reply(200, Headers, <<"Hello world!">>, Req),
    {ok, OutputReq, Opts}.
