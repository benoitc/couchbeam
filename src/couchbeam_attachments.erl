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

%% @doc add attachment  to a doc and encode it. Give possibility to send attachments inline.
-spec add_inline(Doc::map(), Content::binary(), AName::string() | binary()) -> map().
add_inline(Doc, Content, AName) ->
    AName1 = hackney_bstr:to_binary(AName),
    ContentType = mimerl:filename(AName1),
    add_inline(Doc, Content, AName1, ContentType).

%% @doc add attachment  to a doc and encode it with ContentType fixed.
-spec add_inline(Doc::map(), Content::binary(), AName::string() | binary(), ContentType::string() | binary()) -> map().
add_inline(Doc, Content, AName, ContentType) when is_map(Doc) ->
    Data = base64:encode(Content),
    AttachmentData = #{
        <<"content_type">> => ContentType,
        <<"data">> => Data
    },
    
    Attachments = maps:get(<<"_attachments">>, Doc, #{}),
    NewAttachments = Attachments#{AName => AttachmentData},
    Doc#{<<"_attachments">> => NewAttachments}.


add_stub(Doc, Name, ContentType) when is_map(Doc) ->
    AttName = couchbeam_util:to_binary(Name),
    AttData = #{
        <<"content_type">> => couchbeam_util:to_binary(ContentType)
    },
    
    Attachments = maps:get(<<"_attachments">>, Doc, #{}),
    NewAttachments = Attachments#{AttName => AttData},
    Doc#{<<"_attachments">> => NewAttachments}.


%% @doc delete an attachment record in doc. This is different from delete_attachment
%%      change is only applied in Doc object. Save_doc should be save to save changes.
-spec delete_inline(Doc::map(), AName::string() | binary()) -> map().
delete_inline(Doc, AName) when is_list(AName) ->
    delete_inline(Doc, list_to_binary(AName));
delete_inline(Doc, AName) when is_binary(AName), is_map(Doc) ->
    case maps:get(<<"_attachments">>, Doc, undefined) of
        undefined ->
            Doc;
        Attachments when is_map(Attachments) ->
            case maps:is_key(AName, Attachments) of
                false ->
                    Doc;
                true ->
                    NewAttachments = maps:remove(AName, Attachments),
                    Doc#{<<"_attachments">> => NewAttachments}
            end
    end.