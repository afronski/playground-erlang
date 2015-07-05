-module(md5_checksum).

-export([ calculate_md5/1, calculate_md5_partially/1 ]).
-define(CHUNK_SIZE, 1024 * 1024).

calculate_md5(Filename) ->
    {ok, Content} = file:read_file(Filename),
    erlang:md5(Content).

calculate_md5_partially(BigFilename) ->
    Context = erlang:md5_init(),
    FinalContext = build_md5_partially(Context, BigFilename),
    erlang:md5_final(FinalContext).

build_md5_partially(Context, BigFilename) ->
    {ok, File} = file:open(BigFilename, [ read, raw, binary ]),
    handle_part(file:read(File, ?CHUNK_SIZE), File, Context).

handle_part(eof, _File, Context)        -> Context;
handle_part({ok, Data}, File, Context) ->
    AnotherContext = erlang:md5_update(Context, Data),
    handle_part(file:read(File, ?CHUNK_SIZE), File, AnotherContext).

