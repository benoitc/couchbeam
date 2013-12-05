%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%

-module(couchbeam_ejson).

-export([encode/1, decode/1]).

-include("couchbeam.hrl").


-ifndef('WITH_JIFFY').
-define(JSON_ENCODE(D), jsx:encode(D, [{pre_encode, fun jsx_pre_encode/1}])).
-define(JSON_DECODE(D), jsx:decode(D, [{post_decode, fun jsx_post_decode/1}])).

-else.
-define(JSON_ENCODE(D), jiffy:encode(D, [uescape])).
-define(JSON_DECODE(D), jiffy:decode(D)).
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
            throw({invalid_json, Error});
        error:badarg ->
            throw({invalid_json, badarg})
    end.

jsx_pre_encode({[]}) ->
    [{}];
jsx_pre_encode({PropList}) ->
    PropList;
jsx_pre_encode(true) ->
    true;
jsx_pre_encode(false) ->
    false;
jsx_pre_encode(null) ->
    null;
jsx_pre_encode(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
jsx_pre_encode(Term) ->
    Term.

jsx_post_decode([{}]) ->
    {[]};
jsx_post_decode([{_Key, _Value} | _Rest] = PropList) ->
    {PropList};
jsx_post_decode(Term) ->
    Term.
