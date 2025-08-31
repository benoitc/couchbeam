%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Map-based streaming JSON decoder

-module(couchbeam_ejson_stream).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-export([init/0, init/1,
         decode/2, 
         decode_proplist/2,
         set_return_maps/2]).

-record(decoder, {
    return_maps = true :: boolean(),
    stack = [] :: list(),
    current = undefined :: term()
}).

%% @doc Initialize decoder with maps as default
-spec init() -> #decoder{}.
init() ->
    #decoder{return_maps = true}.

%% @doc Initialize decoder with specified options
-spec init(Options::list()) -> #decoder{}.
init(Options) ->
    ReturnMaps = proplists:get_value(return_maps, Options, true),
    #decoder{return_maps = ReturnMaps}.

%% @doc Set whether to return maps or proplists
-spec set_return_maps(Decoder::#decoder{}, ReturnMaps::boolean()) -> #decoder{}.
set_return_maps(Decoder, ReturnMaps) ->
    Decoder#decoder{return_maps = ReturnMaps}.

%% @doc Decode streaming JSON data, returns maps by default
-spec decode(Data::binary(), Decoder::#decoder{}) -> 
    {ok, Value::term(), Rest::binary(), Decoder::#decoder{}} |
    {partial, Decoder::#decoder{}} |
    {error, Reason::term()}.
decode(Data, #decoder{return_maps = true} = Decoder) ->
    decode_stream(Data, Decoder);
decode(Data, #decoder{return_maps = false} = Decoder) ->
    decode_proplist(Data, Decoder).

%% @doc Decode streaming JSON data to proplists (deprecated)
-spec decode_proplist(Data::binary(), Decoder::#decoder{}) ->
    {ok, Value::term(), Rest::binary(), Decoder::#decoder{}} |
    {partial, Decoder::#decoder{}} |
    {error, Reason::term()}.
decode_proplist(Data, Decoder) ->
    case decode_stream(Data, Decoder#decoder{return_maps = false}) of
        {ok, Value, Rest, NewDecoder} ->
            {ok, convert_to_proplist(Value), Rest, NewDecoder};
        Other ->
            Other
    end.

%% Internal streaming decoder
decode_stream(<<>>, Decoder) ->
    {partial, Decoder};

decode_stream(<<${, Rest/binary>>, #decoder{stack = Stack} = Decoder) ->
    decode_object(Rest, Decoder#decoder{
        stack = [object | Stack],
        current = if Decoder#decoder.return_maps -> #{}; true -> [] end
    });

decode_stream(<<$[, Rest/binary>>, #decoder{stack = Stack} = Decoder) ->
    decode_array(Rest, Decoder#decoder{
        stack = [array | Stack],
        current = []
    });

decode_stream(<<$", Rest/binary>>, Decoder) ->
    decode_string(Rest, <<>>, Decoder);

decode_stream(<<C, Rest/binary>>, Decoder) when C >= $0, C =< $9; C =:= $- ->
    decode_number(<<C, Rest/binary>>, <<>>, Decoder);

decode_stream(<<"true", Rest/binary>>, Decoder) ->
    {ok, true, Rest, Decoder};

decode_stream(<<"false", Rest/binary>>, Decoder) ->
    {ok, false, Rest, Decoder};

decode_stream(<<"null", Rest/binary>>, Decoder) ->
    {ok, null, Rest, Decoder};

decode_stream(<<C, Rest/binary>>, Decoder) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
    decode_stream(Rest, Decoder).

%% Object decoder
decode_object(<<$}, Rest/binary>>, #decoder{current = Current, stack = [object | Stack]} = Decoder) ->
    Object = if
        Decoder#decoder.return_maps -> Current;
        true -> {lists:reverse(Current)}
    end,
    complete_value(Object, Rest, Decoder#decoder{stack = Stack, current = undefined});

decode_object(Data, #decoder{current = Current} = Decoder) ->
    case skip_whitespace(Data) of
        <<$", Rest/binary>> ->
            case decode_string(Rest, <<>>, Decoder) of
                {ok, Key, Rest2, _} ->
                    case skip_whitespace(Rest2) of
                        <<$:, Rest3/binary>> ->
                            case decode_stream(skip_whitespace(Rest3), Decoder) of
                                {ok, Value, Rest4, _} ->
                                    NewCurrent = if
                                        Decoder#decoder.return_maps ->
                                            Current#{Key => Value};
                                        true ->
                                            [{Key, Value} | Current]
                                    end,
                                    case skip_whitespace(Rest4) of
                                        <<$,, Rest5/binary>> ->
                                            decode_object(Rest5, Decoder#decoder{current = NewCurrent});
                                        <<$}, Rest5/binary>> ->
                                            Object = if
                                                Decoder#decoder.return_maps -> NewCurrent;
                                                true -> {lists:reverse(NewCurrent)}
                                            end,
                                            complete_value(Object, Rest5, Decoder#decoder{current = undefined});
                                        _ ->
                                            {partial, Decoder}
                                    end;
                                {partial, _} ->
                                    {partial, Decoder};
                                {error, _} = Error ->
                                    Error
                            end;
                        _ ->
                            {partial, Decoder}
                    end;
                {partial, _} ->
                    {partial, Decoder};
                {error, _} = Error ->
                    Error
            end;
        <<>> ->
            {partial, Decoder};
        _ ->
            {error, invalid_object}
    end.

%% Array decoder  
decode_array(<<$], Rest/binary>>, #decoder{current = Current, stack = [array | Stack]} = Decoder) ->
    complete_value(lists:reverse(Current), Rest, Decoder#decoder{stack = Stack, current = undefined});

decode_array(Data, #decoder{current = Current} = Decoder) ->
    case decode_stream(skip_whitespace(Data), Decoder) of
        {ok, Value, Rest, _} ->
            NewCurrent = [Value | Current],
            case skip_whitespace(Rest) of
                <<$,, Rest2/binary>> ->
                    decode_array(Rest2, Decoder#decoder{current = NewCurrent});
                <<$], Rest2/binary>> ->
                    complete_value(lists:reverse(NewCurrent), Rest2, Decoder#decoder{current = undefined});
                _ ->
                    {partial, Decoder}
            end;
        {partial, _} ->
            {partial, Decoder};
        {error, _} = Error ->
            Error
    end.

%% String decoder
decode_string(<<$", Rest/binary>>, Acc, Decoder) ->
    {ok, Acc, Rest, Decoder};
decode_string(<<$\\, $", Rest/binary>>, Acc, Decoder) ->
    decode_string(Rest, <<Acc/binary, $">>, Decoder);
decode_string(<<$\\, $\\, Rest/binary>>, Acc, Decoder) ->
    decode_string(Rest, <<Acc/binary, $\\>>, Decoder);
decode_string(<<$\\, $/, Rest/binary>>, Acc, Decoder) ->
    decode_string(Rest, <<Acc/binary, $/>>, Decoder);
decode_string(<<$\\, $b, Rest/binary>>, Acc, Decoder) ->
    decode_string(Rest, <<Acc/binary, $\b>>, Decoder);
decode_string(<<$\\, $f, Rest/binary>>, Acc, Decoder) ->
    decode_string(Rest, <<Acc/binary, $\f>>, Decoder);
decode_string(<<$\\, $n, Rest/binary>>, Acc, Decoder) ->
    decode_string(Rest, <<Acc/binary, $\n>>, Decoder);
decode_string(<<$\\, $r, Rest/binary>>, Acc, Decoder) ->
    decode_string(Rest, <<Acc/binary, $\r>>, Decoder);
decode_string(<<$\\, $t, Rest/binary>>, Acc, Decoder) ->
    decode_string(Rest, <<Acc/binary, $\t>>, Decoder);
decode_string(<<C, Rest/binary>>, Acc, Decoder) ->
    decode_string(Rest, <<Acc/binary, C>>, Decoder);
decode_string(<<>>, _Acc, Decoder) ->
    {partial, Decoder}.

%% Number decoder
decode_number(<<C, Rest/binary>>, Acc, Decoder) when C >= $0, C =< $9; C =:= $.; C =:= $e; C =:= $E; C =:= $+; C =:= $- ->
    decode_number(Rest, <<Acc/binary, C>>, Decoder);
decode_number(Data, Acc, Decoder) ->
    try
        Number = case binary:match(Acc, <<".">>) of
            nomatch -> binary_to_integer(Acc);
            _ -> binary_to_float(Acc)
        end,
        {ok, Number, Data, Decoder}
    catch
        _:_ ->
            {error, invalid_number}
    end.

%% Complete value based on stack
complete_value(Value, Rest, #decoder{stack = []} = Decoder) ->
    {ok, Value, Rest, Decoder};
complete_value(Value, Rest, #decoder{stack = [object | _], current = Current} = Decoder) ->
    decode_object(Rest, Decoder#decoder{current = Current});
complete_value(Value, Rest, #decoder{stack = [array | _], current = Current} = Decoder) ->
    NewCurrent = [Value | Current],
    decode_array(Rest, Decoder#decoder{current = NewCurrent}).

%% Skip whitespace
skip_whitespace(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
    skip_whitespace(Rest);
skip_whitespace(Data) ->
    Data.

%% Convert map to proplist (for backward compatibility)
convert_to_proplist(Map) when is_map(Map) ->
    {[{K, convert_to_proplist(V)} || {K, V} <- maps:to_list(Map)]};
convert_to_proplist(List) when is_list(List) ->
    [convert_to_proplist(V) || V <- List];
convert_to_proplist(Value) ->
    Value.