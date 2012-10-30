%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%

-module(couchbeam_ejson).

-export([encode/1, decode/1,
         erl_encode/1, erl_decode/1]).

-include("couchbeam.hrl").


-ifndef('WITH_MOCHIJSON').
-define(JSON_ENCODE(D), jiffy:encode(D, [uescape])).
-define(JSON_DECODE(D), jiffy:decode(D)).
-else.
-define(JSON_ENCODE(D), erl_encode(D)).
-define(JSON_DECODE(D), erl_decode(D)).
-endif.


-spec encode(ejson()) -> binary().

%% @doc encode an erlang term to JSON. Throw an exception if there is
%% any error.
encode(D) ->
    ?JSON_ENCODE(D).

-spec decode(binary()) -> ejson().
%% @doc decode a binary to an EJSON term. Throw an exception if there is
%% any error.
decode(D) ->
    try
        ?JSON_DECODE(D)
    catch
        throw:Error ->
            throw({invalid_json, Error})
    end.


erl_decode(D) ->
    try
        (mochijson2:decoder([{object_hook, fun({struct, L}) -> {L}end}]))(D)
    catch _Type:Error ->
        throw({invalid_json, {Error, D}})
    end.

erl_encode(D) ->
    Opts = [{handler, fun mochi_encode_handler/1}],
    iolist_to_binary((mochijson2:encoder(Opts))(D)).

mochi_encode_handler({L}) when is_list(L) ->
    {struct, L};
mochi_encode_handler(Bad) ->
    exit({json_encode, {bad_term, Bad}}).
