%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%

-module(couchbeam_ejson).

-export([encode/1, decode/1]).

-include("couchbeam.hrl").


-ifndef('WITH_JIFFY').
-define(JSON_ENCODE(D), jsx:encode(D)).
-define(JSON_DECODE(D), jsx:decode(D, [{return_maps, true}])).

-else.
-define(JSON_ENCODE(D), jiffy:encode(D, [uescape, use_nil])).
-define(JSON_DECODE(D), jiffy:decode(D, [return_maps])).
-endif.


-spec encode(term()) -> binary().

%% @doc encode an erlang term to JSON. Throw an exception if there is
%% any error.
encode(D) ->
    ?JSON_ENCODE(D).

-spec decode(binary()) -> term().
%% @doc decode a binary to an erlang term (map). Throw an exception if there is
%% any error.
decode(D) ->
    try
        ?JSON_DECODE(D)
    catch
        throw:Error ->
            throw({invalid_json, Error});
        error:badarg ->
            throw({invalid_json, badarg})
    end.