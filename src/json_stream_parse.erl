%%% -*- erlang -*-
%% Simple streaming JSON parser for CouchDB view/changes responses.
%% Parses the response and emits each row/change (as a map) incrementally.
%% Looks for both "rows" (views) and "results" (changes) arrays.

-module(json_stream_parse).

-export([init/0, feed/2, finish/1]).
%% tests
-ifdef(TEST).
-export([parse_rows/1]).
-endif.

-record(st, {
    phase = find_rows :: find_rows | in_rows | done,
    buf = <<>> :: binary(),
    i = 0 :: non_neg_integer(),
    in_string = false :: boolean(),
    escape = false :: boolean(),
    arr_depth = 0 :: non_neg_integer(),
    obj_depth = 0 :: non_neg_integer(),
    obj_start = -1 :: integer()
}).

init() -> #st{}.

%% feed(Data, State) -> {Rows, NewState}
feed(Data, #st{buf=Buf0}=S0) when is_binary(Data) ->
    S = S0#st{buf = <<Buf0/binary, Data/binary>>},
    parse(S, []).

finish(S=#st{phase=done}) -> {[], S};
finish(S) ->
    %% Try parse any remaining complete rows
    {Rows, S1} = parse(S, []),
    {Rows, S1}.

parse(S=#st{phase=find_rows, buf=Buf, i=I}, Acc) ->
    case find_rows_start(Buf, I) of
        not_found -> {lists:reverse(Acc), S#st{i=byte_size(Buf)}};
        {_, ArrIdx} ->
            S1 = S#st{phase=in_rows, i=ArrIdx+1, arr_depth=1},
            parse(S1, Acc)
    end;
parse(S=#st{phase=in_rows, buf=Buf, i=I,
            in_string=Str, escape=Esc,
            arr_depth=AD, obj_depth=OD, obj_start=OS}, Acc) when I < byte_size(Buf) ->
    <<_:I/binary, C, _/binary>> = Buf,
    case Str of
        true ->
            case {Esc, C} of
                {true, _} -> parse(S#st{i=I+1, escape=false}, Acc);
                {_, $\\} -> parse(S#st{i=I+1, escape=true}, Acc);
                {_, $\"} -> parse(S#st{i=I+1, in_string=false}, Acc);
                _ -> parse(S#st{i=I+1}, Acc)
            end;
        false ->
            case C of
                $\" -> parse(S#st{i=I+1, in_string=true}, Acc);
                ${ when OD =:= 0, OS < 0 ->
                    parse(S#st{i=I+1, obj_depth=1, obj_start=I}, Acc);
                ${ -> parse(S#st{i=I+1, obj_depth=OD+1}, Acc);
                $} when OD > 0 ->
                    case OD-1 of
                        0 ->
                            ObjBin = binary:part(Buf, OS, I-OS+1),
                            Row = couchbeam_ejson:decode(ObjBin),
                            S1 = S#st{i=I+1, obj_depth=0, obj_start=-1},
                            parse(S1, [Row|Acc]);
                        N -> parse(S#st{i=I+1, obj_depth=N}, Acc)
                    end;
                $[ -> parse(S#st{i=I+1, arr_depth=AD+1}, Acc);
                $] ->
                    case AD-1 of
                        0 ->
                            %% rows array ended; we can stop
                            %% Drop consumed buffer to current index
                            {lists:reverse(Acc), S#st{phase=done, i=I+1}};
                        N -> parse(S#st{i=I+1, arr_depth=N}, Acc)
                    end;
                _ -> parse(S#st{i=I+1}, Acc)
            end
    end;
parse(S=#st{phase=in_rows}, Acc) -> {lists:reverse(Acc), S};
parse(S, Acc) -> {lists:reverse(Acc), S}.

%% Find the start of the rows/results array: look for "rows" or "results" token then the following '['
find_rows_start(Buf, StartI) ->
    Scope = {StartI, byte_size(Buf)-StartI},
    %% Try to find "rows" (views) or "results" (changes)
    case binary:match(Buf, <<"\"rows\"">>, [{scope, Scope}]) of
        {Pos, _Len} ->
            find_array_after_key(Buf, Pos, 5);
        nomatch ->
            case binary:match(Buf, <<"\"results\"">>, [{scope, Scope}]) of
                {Pos, _Len} ->
                    find_array_after_key(Buf, Pos, 8);
                nomatch ->
                    not_found
            end
    end.

find_array_after_key(Buf, Pos, KeyLen) ->
    case find_next(Buf, Pos+KeyLen+1, $:) of
        not_found -> not_found;
        ColonI ->
            case find_next(Buf, ColonI+1, $[) of
                not_found -> not_found;
                ArrI -> {Pos, ArrI}
            end
    end.

find_next(Buf, I, Char) when I < byte_size(Buf) ->
    <<_:I/binary, C, _/binary>> = Buf,
    if C =:= Char -> I;
       true -> find_next(Buf, I+1, Char)
    end;
find_next(_Buf, _I, _Char) -> not_found.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_rows(Json) ->
    P0 = init(),
    {R1, P1} = feed(Json, P0),
    {R2, _} = finish(P1),
    R1 ++ R2.

basic_chunked_parse_test() ->
    %% Simulate a view JSON response split in chunks
    Row1 = #{<<"id">> => <<"a">>, <<"key">> => <<"a">>, <<"value">> => 1},
    Row2 = #{<<"id">> => <<"b">>, <<"key">> => <<"b">>, <<"value">> => 2},
    BodyMap = #{<<"total_rows">> => 2, <<"offset">> => 0, <<"rows">> => [Row1, Row2]},
    Bin = couchbeam_ejson:encode(BodyMap),
    <<C1:30/binary, C2:25/binary, C3/binary>> = Bin,
    P = init(),
    {Rows0, P1} = feed(C1, P),
    ?assertEqual([], Rows0),
    {Rows1, P2} = feed(C2, P1),
    %% depending on split, may or may not have a row; just ensure maps when present
    lists:foreach(fun(X) -> ?assert(is_map(X)) end, Rows1),
    {Rows2, _} = feed(C3, P2),
    AllRows = Rows1 ++ Rows2,
    ?assertEqual(2, length(AllRows)),
    lists:foreach(fun(X) -> ?assert(is_map(X)) end, AllRows),
    ok.

%% Test parsing changes feed response (uses "results" instead of "rows")
changes_results_parse_test() ->
    Change1 = #{<<"seq">> => 1, <<"id">> => <<"doc1">>, <<"changes">> => [#{<<"rev">> => <<"1-abc">>}]},
    Change2 = #{<<"seq">> => 2, <<"id">> => <<"doc2">>, <<"changes">> => [#{<<"rev">> => <<"1-def">>}]},
    BodyMap = #{<<"results">> => [Change1, Change2], <<"last_seq">> => 2},
    Bin = couchbeam_ejson:encode(BodyMap),
    Results = parse_rows(Bin),
    ?assertEqual(2, length(Results)),
    [R1, R2] = Results,
    ?assertEqual(1, maps:get(<<"seq">>, R1)),
    ?assertEqual(2, maps:get(<<"seq">>, R2)),
    ok.

-endif.
