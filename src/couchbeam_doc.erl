%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_doc).
-author('Benoît Chesneau <benoitc@e-engura.org>').
-include("couchbeam.hrl").

-export([new/0, new/1, new/2,
         set_value/3, get_value/2, get_value/3,
         take_value/2, take_value/3,
         delete_value/2, extend/2, extend/3,
         merge/2, merge/3]).
-export([get_id/1, get_rev/1, get_idrev/1, is_saved/1]).

-type key_val() :: binary().

%% @doc Create a new empty document
-spec new() -> doc().
new() ->
    #{}.

%% @doc Create a new document with an ID
-spec new(DocId::binary()) -> doc().
new(DocId) when is_binary(DocId) ->
    #{<<"_id">> => DocId}.

%% @doc Create a new document with ID and initial values
-spec new(DocId::binary(), Values::map() | list()) -> doc().
new(DocId, Values) when is_binary(DocId), is_map(Values) ->
    Values#{<<"_id">> => DocId};
new(DocId, Values) when is_binary(DocId), is_list(Values) ->
    maps:from_list([{<<"_id">>, DocId} | Values]).

%% @doc get document id.
-spec get_id(Doc::doc()) -> binary() | undefined.
get_id(Doc) when is_map(Doc) ->
    maps:get(<<"_id">>, Doc, undefined).

%% @doc get document revision.
-spec get_rev(Doc::doc()) -> binary() | undefined.
get_rev(Doc) when is_map(Doc) ->
    maps:get(<<"_rev">>, Doc, undefined).

%% @doc get a tuple containing document id and revision.
-spec get_idrev(Doc::doc()) -> {DocId::binary() | undefined, DocRev::binary() | undefined}.
get_idrev(Doc) when is_map(Doc) ->
    {get_id(Doc), get_rev(Doc)}.

%% @doc If document have been saved (revision is defined) return true,
%% else, return false.
-spec is_saved(Doc::doc()) -> boolean().
is_saved(Doc) when is_map(Doc) ->
    maps:is_key(<<"_rev">>, Doc).

%% @doc set a value for a key in document. If key exists it will be updated.
-spec set_value(Key::key_val(), Value::term(), Doc::doc()) -> doc().
set_value(Key, Value, Doc) when is_binary(Key), is_map(Doc) ->
    Doc#{Key => Value}.

%% @doc Returns the value of a simple key/value property in document
%% Equivalent to get_value(Key, Doc, undefined).
-spec get_value(Key::key_val(), Doc::doc()) -> term().
get_value(Key, Doc) ->
    get_value(Key, Doc, undefined).

%% @doc Returns the value of a simple key/value property in document
-spec get_value(Key::key_val(), Doc::doc(), Default::term()) -> term().
get_value(Key, Doc, Default) when is_binary(Key), is_map(Doc) ->
    maps:get(Key, Doc, Default).

%% @doc Returns the value of a simple key/value property in document and deletes
%% it from document
%% Equivalent to take_value(Key, Doc, undefined).
-spec take_value(Key::key_val(), Doc::doc()) -> {term(), doc()}.
take_value(Key, Doc) ->
    take_value(Key, Doc, undefined).

%% @doc Returns the value of a simple key/value property in document and deletes
%% it from document
-spec take_value(Key::key_val(), Doc::doc(), Default::term()) -> {term(), doc()}.
take_value(Key, Doc, Default) when is_binary(Key), is_map(Doc) ->
    case maps:take(Key, Doc) of
        {Value, NewDoc} -> {Value, NewDoc};
        error -> {Default, Doc}
    end.

%% @doc Deletes all entries associated with Key in document.
-spec delete_value(Key::key_val(), Doc::doc()) -> doc().
delete_value(Key, Doc) when is_binary(Key), is_map(Doc) ->
    maps:remove(Key, Doc).

%% @doc extend a document by key, value
-spec extend(Key::binary(), Value::term(), Doc::doc()) -> doc().
extend(Key, Value, Doc) when is_binary(Key), is_map(Doc) ->
    Doc#{Key => Value}.

%% @doc extend a document by a map or list of key-value pairs
-spec extend(Props::map() | list(), Doc::doc()) -> doc().
extend(Props, Doc) when is_map(Props), is_map(Doc) ->
    maps:merge(Doc, Props);
extend(Props, Doc) when is_list(Props), is_map(Doc) ->
    lists:foldl(fun({K, V}, Acc) -> Acc#{K => V} end, Doc, Props).

%% @doc merge two documents
-spec merge(Doc1::doc(), Doc2::doc()) -> doc().
merge(Doc1, Doc2) when is_map(Doc1), is_map(Doc2) ->
    maps:merge(Doc1, Doc2).

%% @doc merge two documents with a merge function
-spec merge(Fun::fun((Key::binary(), V1::term(), V2::term()) -> term()),
            Doc1::doc(), Doc2::doc()) -> doc().
merge(Fun, Doc1, Doc2) when is_function(Fun, 3), is_map(Doc1), is_map(Doc2) ->
    maps:merge_with(Fun, Doc1, Doc2).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_value_test() ->
    Doc = #{<<"a">> => 1},
    ?assertEqual(1, couchbeam_doc:get_value(<<"a">>, Doc)),
    ?assertEqual(undefined, couchbeam_doc:get_value(<<"b">>, Doc)),
    ?assertEqual(nil, couchbeam_doc:get_value(<<"b">>, Doc, nil)),
    ok.

set_value_test() ->
    Doc = #{<<"a">> => 1},
    ?assertEqual(#{<<"a">> => 1, <<"b">> => 2},
                 couchbeam_doc:set_value(<<"b">>, 2, Doc)),
    ?assertEqual(#{<<"a">> => 3},
                 couchbeam_doc:set_value(<<"a">>, 3, Doc)),
    ok.

delete_value_test() ->
    Doc = #{<<"a">> => 1},
    ?assertEqual(#{}, couchbeam_doc:delete_value(<<"a">>, Doc)),
    ok.

extend_test() ->
    Doc = #{<<"a">> => 1},
    ?assertEqual(#{<<"a">> => 1, <<"b">> => 2},
                 couchbeam_doc:extend(<<"b">>, 2, Doc)),
    ?assertEqual(#{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3},
                 couchbeam_doc:extend(#{<<"b">> => 2, <<"c">> => 3}, Doc)),
    ok.

take_value_test() ->
    Doc = #{<<"a">> => 1, <<"b">> => 2},
    {V1, Doc1} = couchbeam_doc:take_value(<<"a">>, Doc),
    ?assertEqual(1, V1),
    ?assertEqual(#{<<"b">> => 2}, Doc1),
    {V2, Doc2} = couchbeam_doc:take_value(<<"c">>, Doc, nil),
    ?assertEqual(nil, V2),
    ?assertEqual(Doc, Doc2),
    ok.

-endif.