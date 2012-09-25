%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%

-module(couchbeam_ejson).

-export([encode/1, decode/1]).

-include("couchbeam.hrl").

-spec encode(ejson()) -> binary().
%% @doc encode an erlang term to JSON. Throw an exception if there is
%% any error.
encode(D) ->
    try
        jiffy:encode(D, [uescape])
    catch
        error:{not_loaded, _} ->
            erl_encode(D);
        error:{load_failed, _} ->
            erl_encode(D)
    end.

-spec decode(binary()) -> ejson().
%% @doc decode a binary to an EJSON term. Throw an exception if there is
%% any error.
decode(D) ->
    try
        jiffy:decode(D)
    catch
        error:{not_loaded, _} ->
            erl_decode(D);
        error:{load_failed, _} ->
            erl_decode(D);
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
