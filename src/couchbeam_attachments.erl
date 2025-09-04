%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.


%% @doc This module contains utilities to manage attachments

-module(couchbeam_attachments).

-include("couchbeam.hrl").

-export([add_inline/3, add_inline/4,
         add_stub/3,
        delete_inline/2]).
 
-spec add_inline(doc(), iodata(), string() | binary()) -> doc().
%% @doc add attachment  to a doc and encode it. Give possibility to send attachments inline.
add_inline(Doc, Content, AName) ->
    AName1 = hackney_bstr:to_binary(AName),
    ContentType = mimerl:filename(AName1),
    add_inline(Doc, Content, AName1, ContentType).

-spec add_inline(doc(), iodata(), string() | binary(), string() | binary()) -> doc().
%% @doc add attachment  to a doc and encode it with ContentType fixed.
add_inline(Doc, Content, AName, ContentType) ->
    Data = base64:encode(Content),
    NewAtt = #{<<"content_type">> => ContentType,
               <<"data">> => Data},
    Atts0 = maps:get(<<"_attachments">>, Doc, #{}),
    Atts1 = maps:put(AName, NewAtt, Atts0),
    couchbeam_doc:set_value(<<"_attachments">>, Atts1, Doc).


-spec add_stub(doc(), string() | binary(), string() | binary()) -> doc().
add_stub(Doc, Name, ContentType) ->
    AttName = couchbeam_util:to_binary(Name),
    Att = #{<<"content_type">> => couchbeam_util:to_binary(ContentType)},
    Atts0 = maps:get(<<"_attachments">>, Doc, #{}),
    Atts1 = maps:put(AttName, Att, Atts0),
    couchbeam_doc:set_value(<<"_attachments">>, Atts1, Doc).


-spec delete_inline(doc(), string() | binary()) -> doc().
%% @doc delete an attachment record in doc. This is different from delete_attachment
%%      change is only applied in Doc object. Save_doc should be save to save changes.
delete_inline(Doc, AName) when is_list(AName) ->
    delete_inline(Doc, list_to_binary(AName));
delete_inline(Doc, AName) when is_binary(AName) ->
    case maps:get(<<"_attachments">>, Doc, undefined) of
        undefined -> Doc;
        Atts when is_map(Atts) ->
            case maps:is_key(AName, Atts) of
                false -> Doc;
                true ->
                    Atts1 = maps:remove(AName, Atts),
                    couchbeam_doc:set_value(<<"_attachments">>, Atts1, Doc)
            end
    end.

% no-op legacy placeholder removed
