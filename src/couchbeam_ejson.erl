%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%

-module(couchbeam_ejson).

-export([encode/1, decode/1]).
-export([post_decode/1]).

-include("couchbeam.hrl").


-ifndef('WITH_JIFFY').
-define(JSON_ENCODE(D), jsx:encode(pre_encode(D))).
-define(JSON_DECODE(D), post_decode(jsx:decode(D))).

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

post_decode({[{}]}) ->
    {[]};
post_decode([{}]) ->
    {[]};
post_decode([{_Key, _Value} | _Rest] = PropList) ->
    {[ {Key, post_decode(Value)} || {Key, Value} <- PropList ]};
post_decode(List) when is_list(List) ->
    [ post_decode(Term) || Term <- List];
post_decode({Term}) ->
    post_decode(Term);
post_decode(Term) ->
    Term.
