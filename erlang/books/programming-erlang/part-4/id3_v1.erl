-module(id3_v1).

-import(lists, [ filter/2, map/2, reverse/1 ]).
-export([ dir/1, read_id3_tag/1 ]).

dir(Dir) ->
    Files = lib_find:files(Dir, "*.mp3", true),
    L1 = map(fun(I) -> {I, (catch read_id3_tag(I))} end,  Files),

    %% L1 = [{File, Parse}] where Parse = error | [{Tag,Val}]
    %% we now have to remove all the entries from L where
    %% Parse = error. We can do this with a filter operation

    L2 = filter(fun({_,error}) -> false;
                   (_) -> true
                end, L1),
    lib_misc:dump("mp3data", L2).

read_id3_tag(File) ->
    case file:open(File, [ read, binary, raw ]) of
        {ok, S} ->
            Size = filelib:file_size(File),
            {ok, B2} = file:pread(S, Size - 128, 128),
            Result = parse_v1_tag(B2),
            file:close(S),
            Result;

        _Error ->
            error
    end.

parse_v1_tag(<<$T,$A,$G,
              Title:30/binary, Artist:30/binary,
              Album:30/binary, _Year:4/binary,
              _Comment:28/binary, 0:8,Track:8, _Genre:8>>) ->
    {"ID3v1.1",
     [ {track,Track}, {title,trim(Title)}, {artist,trim(Artist)}, {album, trim(Album)} ]};
parse_v1_tag(<<$T,$A,$G,
              Title:30/binary, Artist:30/binary,
              Album:30/binary, _Year:4/binary,
              _Comment:30/binary,_Genre:8>>) ->
    {"ID3v1",
     [ {title,trim(Title)}, {artist,trim(Artist)}, {album, trim(Album)} ]};
parse_v1_tag(_) ->
    error.

trim(Bin) ->
    list_to_binary(trim_blanks(binary_to_list(Bin))).

trim_blanks(X) -> reverse(skip_blanks_and_zero(reverse(X))).

skip_blanks_and_zero([$\s|T]) -> skip_blanks_and_zero(T);
skip_blanks_and_zero([0|T])   -> skip_blanks_and_zero(T);
skip_blanks_and_zero(X)       -> X.
