%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%

-module(couchbeam_ejson).

-export([encode/1, decode/1]).
-export([post_decode/1]).

-include("couchbeam.hrl").


%% Migrate to Erlang/OTP 28 stdlib json module using maps.
%% Objects are represented as maps; arrays as lists.


-spec encode(ejson()) -> binary().

%% @doc encode an erlang term to JSON. Throw an exception if there is
%% any error.
encode(D) ->
    %% json:encode returns iodata(); convert to binary
    iolist_to_binary(json:encode(D)).

-spec decode(binary()) -> ejson().
%% @doc decode a binary to an EJSON term. Throw an exception if there is
%% any error.
decode(D) when is_binary(D) ->
    try
        json:decode(D)
    catch
        error:Reason ->
            throw({invalid_json, Reason})
    end;
decode(D) ->
    decode(iolist_to_binary(D)).
%% post_decode was used to convert jsx proplists to ejson; now it is identity.
post_decode(Term) ->
    Term.
