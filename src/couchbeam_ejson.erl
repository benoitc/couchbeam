%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%

-module(couchbeam_ejson).

-export([encode/1, decode/1, decode/2]).

-include("couchbeam.hrl").


-ifndef('WITH_JIFFY').
-define(JSON_ENCODE(D), jsx:encode(map_or_pre_encode(D))).
-define(JSON_DECODE(D, DecodeOptions), map_or_post_decode(jsx:decode(D, DecodeOptions))).

-else.
-define(JSON_ENCODE(D), jiffy:encode(D, [uescape])).
-define(JSON_DECODE(D, DecodeOptions), jiffy:decode(D, DecodeOptions)).
-endif.


-spec encode(ejson()) -> binary().

%% @doc encode an erlang term to JSON. Throw an exception if there is
%% any error.
encode(D) ->
    ?JSON_ENCODE(D).

-spec decode(binary()) -> ejson().
%% @doc decode a binary to an EJSON term. Throw an exception if there is
%% any error.
decode(D) -> decode(D, []).
decode(D, Options) ->
    DecodeOptions = case proplists:get_value(return_maps, Options) == true of
        true -> [return_maps];
        false -> []
    end,
    try
        ?JSON_DECODE(D, DecodeOptions)
    catch
        throw:Error ->
            throw({invalid_json, Error});
        error:badarg ->
            throw({invalid_json, badarg})
    end.


-ifdef('MAPS_SUPPORT').
map_or_pre_encode(Map = #{}) ->
    Map;
map_or_pre_encode(NotMap) ->
    pre_encode(NotMap).
-else.
map_or_pre_encode(NotMap) ->
    pre_encode(NotMap).
-endif.

pre_encode({[]}) ->
    [{}];
pre_encode({PropList}) ->
    pre_encode(PropList);
pre_encode([{_, _}|_] = PropList) ->
    [ {Key, pre_encode(Value)} || {Key, Value} <- PropList ];
pre_encode(List) when is_list(List) ->
    [ pre_encode(Term) || Term <- List ];
pre_encode(true) ->
    true;
pre_encode(false) ->
    false;
pre_encode(null) ->
    null;
pre_encode(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
pre_encode(Term) when is_integer(Term); is_float(Term); is_binary(Term) ->
    Term.

-ifdef('MAPS_SUPPORT').
map_or_post_decode(Map = #{}) ->
    Map;
map_or_post_decode(NotMap) ->
    post_decode(NotMap).
-else.
map_or_post_decode(NotMap) ->
    post_decode(NotMap).
-endif.

post_decode([{}]) ->
    {[]};
post_decode([{_Key, _Value} | _Rest] = PropList) ->
    {[ {Key, post_decode(Value)} || {Key, Value} <- PropList ]};
post_decode(List) when is_list(List) ->
    [ post_decode(Term) || Term <- List];
post_decode(Term) ->
    Term.
