%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_doc).
-author('Benoît Chesneau <benoitc@e-engura.org>').
-include("couchbeam.hrl").

-export([set_value/3, get_value/2, get_value/3,
         take_value/2, take_value/3,
         delete_value/2, extend/2, extend/3]).
-export([get_id/1, get_rev/1, get_idrev/1, is_saved/1]).

%% @spec get_id(Doc::json_obj()) -> binary()
%% @doc get document id.
get_id(Doc) ->
    get_value(<<"_id">>, Doc).

%% @spec get_rev(Doc::json_obj()) -> binary()
%% @doc get document revision.
get_rev(Doc) ->
    get_value(<<"_rev">>, Doc).

%% @spec get_idrev(Doc::json_obj()) -> {DocId, DocRev}
%% @doc get  a tuple containing docucment id and revision.
get_idrev(Doc) ->
    DocId = get_value(<<"_id">>, Doc),
    DocRev = get_value(<<"_rev">>, Doc),
    {DocId, DocRev}.

%% @spec is_saved(Doc::json_obj()) -> boolean()
%% @doc If document have been saved (revision is defined) return true,
%% else, return false.
is_saved(Doc) ->
    case get_value(<<"_rev">>, Doc) of
        undefined -> false;
        _ -> true
    end.

%% @spec set_value(Key::key_val(), Value::term(), JsonObj::json_obj()) -> term()
%% @doc set a value for a key in jsonobj. If key exists it will be updated.
set_value(Key, Value, JsonObj) when is_list(Key)->
    set_value(list_to_binary(Key), Value, JsonObj);
set_value(Key, Value, JsonObj) when is_binary(Key) ->
    {Props} = JsonObj,
    case proplists:is_defined(Key, Props) of
        true -> set_value1(Props, Key, Value, []);
        false-> {lists:reverse([{Key, Value}|lists:reverse(Props)])}
    end.

%% @spec get_value(Key::key_val(), JsonObj::json_obj()) -> term()
%% @type key_val() = lis() | binary()
%% @doc Returns the value of a simple key/value property in json object
%% Equivalent to get_value(Key, JsonObj, undefined).
get_value(Key, JsonObj) ->
    get_value(Key, JsonObj, undefined).


%% @spec get_value(Key::lis() | binary(), JsonObj::json_obj(), Default::term()) -> term()
%% @doc Returns the value of a simple key/value property in json object
%% function from erlang_couchdb
get_value(Key, JsonObj, Default) when is_list(Key) ->
    get_value(list_to_binary(Key), JsonObj, Default);
get_value(Key, JsonObj, Default) when is_binary(Key) ->
    {Props} = JsonObj,
    couchbeam_util:get_value(Key, Props, Default).


%% @spec take_value(Key::key_val(), JsonObj::json_obj()) -> {term(), json_obj()}
%% @doc Returns the value of a simple key/value property in json object and deletes
%% it form json object
%% Equivalent to take_value(Key, JsonObj, undefined).
take_value(Key, JsonObj) ->
    take_value(Key, JsonObj, undefined).


%% @spec take_value(Key::key_val() | binary(), JsonObj::json_obj(),
%% Default::term()) ->  {term(), json_obj()}
%% @doc Returns the value of a simple key/value property in json object and deletes
%% it from json object
take_value(Key, JsonObj, Default) when is_list(Key) ->
    get_value(list_to_binary(Key), JsonObj, Default);
take_value(Key, JsonObj, Default) when is_binary(Key) ->
    {Props} = JsonObj,
    case lists:keytake(Key, 1, Props) of
        {value, {Key, Value}, Rest} ->
            {Value, {Rest}};
        false ->
            {Default, JsonObj}
    end.

%% @spec delete_value(Key::key_val(), JsonObj::json_obj()) -> json_obj()
%% @doc Deletes all entries associated with Key in json object.
delete_value(Key, JsonObj) when is_list(Key) ->
    delete_value(list_to_binary(Key), JsonObj);
delete_value(Key, JsonObj) when is_binary(Key) ->
    {Props} = JsonObj,
    Props1 = proplists:delete(Key, Props),
    {Props1}.

%% @spec extend(Key::binary(), Value::json_term(), JsonObj::json_obj()) -> json_obj()
%% @doc extend a jsonobject by key, value
extend(Key, Value, JsonObj) ->
    extend({Key, Value}, JsonObj).

%% @spec extend(Prop::property(), JsonObj::json_obj()) -> json_obj()
%% @type property() = json_obj() | tuple()
%% @doc extend a jsonobject by a property, list of property or another jsonobject
extend([], JsonObj) ->
    JsonObj;
extend({List}, JsonObj) when is_list(List)  ->
    extend(List, JsonObj);
extend([Prop|R], JsonObj)->
    NewObj = extend(Prop, JsonObj),
    extend(R, NewObj);
extend({Key, Value}, JsonObj) ->
    set_value(Key, Value, JsonObj).

%% @private
set_value1([], _Key, _Value, Acc) ->
    {lists:reverse(Acc)};
set_value1([{K, V}|T], Key, Value, Acc) ->
    Acc1 = if
        K =:= Key ->
            [{Key, Value}|Acc];
        true ->
            [{K, V}|Acc]
        end,
    set_value1(T, Key, Value, Acc1).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_value_test() ->
    Doc = {[{<<"a">>, 1}]},
    ?assertEqual(1, couchbeam_doc:get_value(<<"a">>, Doc)),
    ?assertEqual(1, couchbeam_doc:get_value("a", Doc)),
    ?assertEqual(undefined, couchbeam_doc:get_value("b", Doc)),
    ?assertEqual(nil, couchbeam_doc:get_value("b", Doc, nil)),
    ok.

set_value_test() ->
    Doc = {[{<<"a">>, 1}]},
    ?assertEqual(undefined, couchbeam_doc:get_value("b", Doc)),
    Doc1 = couchbeam_doc:set_value("b", 1, Doc),
    ?assertEqual(1, couchbeam_doc:get_value("b", Doc1)),
    Doc2 = couchbeam_doc:set_value("b", 0, Doc1),
    ?assertEqual(0, couchbeam_doc:get_value("b", Doc2)),
    ok.

delete_value_test() ->
    Doc = {[{<<"a">>, 1}, {<<"b">>, 1}]},
    Doc1 = couchbeam_doc:delete_value("b", Doc),
    ?assertEqual(undefined, couchbeam_doc:get_value("b", Doc1)),
    ok.

extend_test() ->
    Doc = {[{<<"a">>, 1}]},
    ?assertEqual(1, couchbeam_doc:get_value("a", Doc)),
    ?assertEqual(undefined, couchbeam_doc:get_value("b", Doc)),
    ?assertEqual(undefined, couchbeam_doc:get_value("c", Doc)),
    Doc1 = couchbeam_doc:extend([{<<"b">>, 1}, {<<"c">>, 1}], Doc),
    ?assertEqual(1, couchbeam_doc:get_value("b", Doc1)),
    ?assertEqual(1, couchbeam_doc:get_value("c", Doc1)),
    Doc2 = couchbeam_doc:extend([{<<"b">>, 3}, {<<"d">>, 1}], Doc1),
    ?assertEqual(3, couchbeam_doc:get_value("b", Doc2)),
    ?assertEqual(1, couchbeam_doc:get_value("d", Doc2)),
    ok.

id_rev_test() ->
    Doc = {[{<<"a">>, 1}]},
    ?assertEqual(undefined, couchbeam_doc:get_id(Doc)),
    ?assertEqual(undefined, couchbeam_doc:get_rev(Doc)),
    ?assertEqual({undefined, undefined}, couchbeam_doc:get_idrev(Doc)),
    Doc1 = couchbeam_doc:extend([{<<"_id">>, 1}, {<<"_rev">>, 1}], Doc),
    ?assertEqual(1, couchbeam_doc:get_id(Doc1)),
    ?assertEqual(1, couchbeam_doc:get_rev(Doc1)),
    ?assertEqual({1, 1}, couchbeam_doc:get_idrev(Doc1)),
    ok.

is_saved_test() ->
    Doc = {[{<<"a">>, 1}]},
    ?assertEqual(false, couchbeam_doc:is_saved(Doc)),
    Doc1 = couchbeam_doc:set_value(<<"_rev">>, <<"x">>, Doc),
    ?assertEqual(true, couchbeam_doc:is_saved(Doc1)),
    ok.


take_value_test() ->
    Doc = {[{<<"a">>, 1}, {<<"b">>, 2}]},
    ?assertEqual({undefined, Doc}, couchbeam_doc:take_value(<<"c">>, Doc)),
    ?assertEqual({1, {[{<<"b">>, 2}]}}, couchbeam_doc:take_value(<<"a">>, Doc)),
    ok.

-endif.


