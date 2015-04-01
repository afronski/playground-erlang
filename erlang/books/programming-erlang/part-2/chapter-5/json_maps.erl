-module(json_maps).
-export([ stringify/1, stringify_list/1, parse/1 ]).

% Book was too optimistic about built-in JSON support in ERTS. There
% are no `maps:to_json/1` and `maps:from_json/1` functions and they
% will not be implemented in future.
%
% Details: http://erlang.org/pipermail/erlang-questions/2014-February/076936.html 

stringify(M) ->
    maps:to_json(M).

stringify_list(L) ->
    maps:to_json(maps:from_list(L)).

parse(Json) ->
    maps:from_json(Json).
