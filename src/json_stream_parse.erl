%%% -*- erlang -*-
%% Simple streaming JSON row parser for CouchDB view responses.
%% Parses the response and emits each row (as a map) incrementally.

-module(json_stream_parse).

-export([init/0, feed/2, finish/1]).

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

%% Find the start of the rows array: look for the token "rows" then the following '['
find_rows_start(Buf, StartI) ->
    case binary:match(Buf, <<"\"rows\"">>, [{scope, {StartI, byte_size(Buf)-StartI}}]) of
        nomatch -> not_found;
        {Pos, _Len} ->
            %% find ':' then '[' after it
            case find_next(Buf, Pos+5, $:) of
                not_found -> not_found;
                ColonI ->
                    case find_next(Buf, ColonI+1, $[) of
                        not_found -> not_found;
                        ArrI -> {Pos, ArrI}
                    end
            end
    end.

find_next(Buf, I, Char) when I < byte_size(Buf) ->
    <<_:I/binary, C, _/binary>> = Buf,
    if C =:= Char -> I;
       true -> find_next(Buf, I+1, Char)
    end;
find_next(_Buf, _I, _Char) -> not_found.
