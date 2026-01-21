%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%

-module(couchbeam_ejson).

-export([encode/1, decode/1]).
-export([post_decode/1]).

-include("couchbeam.hrl").


%% JSON handling uses Erlang/OTP stdlib json (OTP 27+) with maps.
%% JSON objects are represented as maps; arrays as lists.


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

%% post_decode was previously used to convert jsx proplists to ejson format.
%% Since objects are now maps, it is identity.
post_decode(Term) ->
    Term.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test encoding maps
encode_map_test() ->
    %% Simple map
    ?assertEqual(<<"{\"a\":1}">>, encode(#{<<"a">> => 1})),

    %% Nested map
    Nested = #{<<"outer">> => #{<<"inner">> => <<"value">>}},
    Encoded = encode(Nested),
    ?assert(is_binary(Encoded)),
    %% Decode to verify round-trip
    ?assertEqual(Nested, decode(Encoded)),

    %% Empty map
    ?assertEqual(<<"{}">>, encode(#{})),
    ok.

%% Test encoding lists
encode_list_test() ->
    ?assertEqual(<<"[1,2,3]">>, encode([1, 2, 3])),
    ?assertEqual(<<"[]">>, encode([])),
    ?assertEqual(<<"[\"a\",\"b\"]">>, encode([<<"a">>, <<"b">>])),
    ok.

%% Test encoding various types
encode_types_test() ->
    %% Strings
    ?assertEqual(<<"\"hello\"">>, encode(<<"hello">>)),

    %% Numbers
    ?assertEqual(<<"42">>, encode(42)),
    ?assertEqual(<<"3.14">>, encode(3.14)),

    %% Booleans
    ?assertEqual(<<"true">>, encode(true)),
    ?assertEqual(<<"false">>, encode(false)),

    %% Null
    ?assertEqual(<<"null">>, encode(null)),
    ok.

%% Test decoding
decode_test() ->
    %% Object to map
    ?assertEqual(#{<<"key">> => <<"value">>}, decode(<<"{\"key\":\"value\"}">>)),

    %% Array to list
    ?assertEqual([1, 2, 3], decode(<<"[1,2,3]">>)),

    %% Nested structure
    Json = <<"{\"users\":[{\"name\":\"alice\"},{\"name\":\"bob\"}]}">>,
    Expected = #{<<"users">> => [#{<<"name">> => <<"alice">>}, #{<<"name">> => <<"bob">>}]},
    ?assertEqual(Expected, decode(Json)),
    ok.

%% Test decode with iolist input
decode_iolist_test() ->
    IoList = [<<"{\"a\":">>, <<"1}">>],
    ?assertEqual(#{<<"a">> => 1}, decode(IoList)),
    ok.

%% Test invalid JSON throws
decode_invalid_test() ->
    ?assertThrow({invalid_json, _}, decode(<<"not json">>)),
    ?assertThrow({invalid_json, _}, decode(<<"{incomplete">>)),
    ok.

%% Test round-trip
roundtrip_test() ->
    Doc = #{
        <<"_id">> => <<"test-doc">>,
        <<"type">> => <<"test">>,
        <<"count">> => 42,
        <<"active">> => true,
        <<"tags">> => [<<"a">>, <<"b">>, <<"c">>],
        <<"metadata">> => #{
            <<"created">> => <<"2024-01-01">>,
            <<"author">> => <<"test">>
        }
    },
    Encoded = encode(Doc),
    Decoded = decode(Encoded),
    ?assertEqual(Doc, Decoded),
    ok.

%% Test post_decode is identity
post_decode_test() ->
    Term = #{<<"key">> => <<"value">>},
    ?assertEqual(Term, post_decode(Term)),
    ?assertEqual([1, 2, 3], post_decode([1, 2, 3])),
    ok.

-endif.
