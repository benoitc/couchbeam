%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%

-module(couchbeam_ejson).

-export([encode/1, decode/1]).

-include("couchbeam.hrl").

%% Use the Erlang/OTP 28+ built-in json library


-spec encode(term()) -> binary().

%% @doc encode an erlang term to JSON. Throw an exception if there is
%% any error.
encode(D) ->
    iolist_to_binary(json:encode(D)).

-spec decode(binary()) -> term().
%% @doc decode a binary to an erlang term (map). Throw an exception if there is
%% any error.
decode(D) ->
    try
        json:decode(D)
    catch
        throw:Error ->
            throw({invalid_json, Error});
        error:badarg ->
            throw({invalid_json, badarg})
    end.
