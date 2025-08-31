%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%% 
%%% Map-based document API

-module(couchbeam_doc_map).
-author('Benoît Chesneau <benoitc@e-engura.org>').
-include("couchbeam.hrl").
-include("couchbeam_maps.hrl").

-export([new/0, new/1, new/2,
         set_value/3, get_value/2, get_value/3,
         take_value/2, take_value/3,
         delete_value/2, extend/2, extend/3,
         merge/2, merge/3]).
-export([get_id/1, get_rev/1, get_idrev/1, is_saved/1]).
-export([from_proplist/1, to_proplist/1]).

%% @doc Create a new empty document
-spec new() -> doc_map().
new() ->
    #{}.

%% @doc Create a new document with an ID
-spec new(DocId::binary()) -> doc_map().
new(DocId) when is_binary(DocId) ->
    #{<<"_id">> => DocId}.

%% @doc Create a new document with ID and initial values
-spec new(DocId::binary(), Values::map() | list()) -> doc_map().
new(DocId, Values) when is_binary(DocId), is_map(Values) ->
    Values#{<<"_id">> => DocId};
new(DocId, Values) when is_binary(DocId), is_list(Values) ->
    maps:from_list([{<<"_id">>, DocId} | Values]).

%% @doc Get document ID
-spec get_id(Doc::doc_map()) -> binary() | undefined.
get_id(Doc) when is_map(Doc) ->
    maps:get(<<"_id">>, Doc, undefined).

%% @doc Get document revision
-spec get_rev(Doc::doc_map()) -> binary() | undefined.
get_rev(Doc) when is_map(Doc) ->
    maps:get(<<"_rev">>, Doc, undefined).

%% @doc Get tuple containing document ID and revision
-spec get_idrev(Doc::doc_map()) -> {DocId::binary() | undefined, DocRev::binary() | undefined}.
get_idrev(Doc) when is_map(Doc) ->
    {get_id(Doc), get_rev(Doc)}.

%% @doc Check if document has been saved (has a revision)
-spec is_saved(Doc::doc_map()) -> boolean().
is_saved(Doc) when is_map(Doc) ->
    maps:is_key(<<"_rev">>, Doc).

%% @doc Set a value for a key in document
-spec set_value(Key::binary(), Value::json_term(), Doc::doc_map()) -> doc_map().
set_value(Key, Value, Doc) when is_binary(Key), is_map(Doc) ->
    Doc#{Key => Value}.

%% @doc Get value from document
-spec get_value(Key::binary(), Doc::doc_map()) -> json_term() | undefined.
get_value(Key, Doc) when is_binary(Key), is_map(Doc) ->
    maps:get(Key, Doc, undefined).

%% @doc Get value from document with default
-spec get_value(Key::binary(), Doc::doc_map(), Default::json_term()) -> json_term().
get_value(Key, Doc, Default) when is_binary(Key), is_map(Doc) ->
    maps:get(Key, Doc, Default).

%% @doc Take value from document (get and remove)
-spec take_value(Key::binary(), Doc::doc_map()) -> {json_term() | undefined, doc_map()}.
take_value(Key, Doc) when is_binary(Key), is_map(Doc) ->
    maps:take(Key, Doc).

%% @doc Take value from document with default
-spec take_value(Key::binary(), Doc::doc_map(), Default::json_term()) -> {json_term(), doc_map()}.
take_value(Key, Doc, Default) when is_binary(Key), is_map(Doc) ->
    case maps:take(Key, Doc) of
        {Value, NewDoc} -> {Value, NewDoc};
        error -> {Default, Doc}
    end.

%% @doc Delete value from document
-spec delete_value(Key::binary(), Doc::doc_map()) -> doc_map().
delete_value(Key, Doc) when is_binary(Key), is_map(Doc) ->
    maps:remove(Key, Doc).

%% @doc Extend document with new values
-spec extend(Values::map() | list(), Doc::doc_map()) -> doc_map().
extend(Values, Doc) when is_map(Values), is_map(Doc) ->
    maps:merge(Doc, Values);
extend(Values, Doc) when is_list(Values), is_map(Doc) ->
    maps:merge(Doc, maps:from_list(Values)).

%% @doc Extend document with new values using merge function
-spec extend(Fun::fun((Key::binary(), V1::json_term(), V2::json_term()) -> json_term()),
             Values::map() | list(), Doc::doc_map()) -> doc_map().
extend(Fun, Values, Doc) when is_function(Fun, 3), is_map(Values), is_map(Doc) ->
    maps:merge_with(Fun, Doc, Values);
extend(Fun, Values, Doc) when is_function(Fun, 3), is_list(Values), is_map(Doc) ->
    maps:merge_with(Fun, Doc, maps:from_list(Values)).

%% @doc Merge two documents
-spec merge(Doc1::doc_map(), Doc2::doc_map()) -> doc_map().
merge(Doc1, Doc2) when is_map(Doc1), is_map(Doc2) ->
    maps:merge(Doc1, Doc2).

%% @doc Merge two documents with merge function
-spec merge(Fun::fun((Key::binary(), V1::json_term(), V2::json_term()) -> json_term()),
            Doc1::doc_map(), Doc2::doc_map()) -> doc_map().
merge(Fun, Doc1, Doc2) when is_function(Fun, 3), is_map(Doc1), is_map(Doc2) ->
    maps:merge_with(Fun, Doc1, Doc2).

%% @doc Convert proplist-based document to map
-spec from_proplist({[{binary(), json_term()}]}) -> doc_map().
from_proplist({Props}) when is_list(Props) ->
    convert_proplist_to_map(Props).

%% @doc Convert map-based document to proplist format
-spec to_proplist(doc_map()) -> {[{binary(), json_term()}]}.
to_proplist(Doc) when is_map(Doc) ->
    {maps:to_list(Doc)}.

%% Internal conversion functions
convert_proplist_to_map(Props) when is_list(Props) ->
    maps:from_list([{K, convert_value(V)} || {K, V} <- Props]).

convert_value({Props}) when is_list(Props) ->
    convert_proplist_to_map(Props);
convert_value(L) when is_list(L) ->
    [convert_value(V) || V <- L];
convert_value(V) ->
    V.