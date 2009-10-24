%%% Copyright 2009 Benoît Chesneau.
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%% @author Benoît Chesneau <benoitc@e-engura.org>
%% @copyright 2009 Benoît Chesneau.

-module(couchbeam_doc).
-author('Benoît Chesneau <benoitc@e-engura.org>').
-include("couchbeam.hrl").

-export([set_value/3, get_value/2, delete_value/2, extend/2, extend/3,
        add_attachment/3, add_attachment/4, delete_inline_attachment/2]).

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
%% @doc get value from a json obje
%% function from erlang_couchdb
get_value(Key, JsonObj) when is_list(Key) ->
    get_value1(Key, JsonObj);
get_value(Key, JsonObj) when is_binary(Key) ->
    {Props} = JsonObj,
    proplists:get_value(Key, Props).
    

%% @spec delete_value(Key::key_val(), JsonObj::json_obj()) -> json_obj()
%% @type key_val() = lis() | binary()
%% @docDeletes all entries associated with Key in json object.  
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
%% @doc extend a jsonobject by a property or list of property
extend({Prop}, JsonObj) ->
    extend(Prop, JsonObj);
extend(Prop, JsonObj) when is_list(Prop)->
    extend1(Prop, JsonObj);
extend(Prop, JsonObj) when is_tuple(Prop)->
    {Props} = JsonObj,
    {lists:reverse([Prop|lists:reverse(Props)])}.

%% @spec add_attachment(Doc::json_obj(),Content::attachment_content(), 
%%      AName::string()) -> json_obj()
%% @doc add attachment  to a doc and encode it. Give possibility to send attachments inline.
add_attachment(Doc, Content, AName) ->
    ContentType = couchbeam_util:guess_mime(AName),
    add_attachment(Doc, Content, AName, ContentType).

%% @spec add_attachment(Doc::json_obj(), Content::attachment_content(),
%%      AName::string(), ContentType::string()) -> json_obj()
%% @doc add attachment  to a doc and encode it with ContentType fixed.    
add_attachment(Doc, Content, AName, ContentType) ->
    {Props} = Doc,
    Data = couchbeam_util:encodeBase64(Content),
    Attachment = {list_to_binary(AName), {[{<<"content-type">>, 
        list_to_binary(ContentType)}, {<<"data">>, Data}]}},
    
    Attachments1 = case proplists:get_value(<<"_attachments">>, Props) of
        undefined -> 
            [Attachment];
        {Attachments} ->
            case set_attachment(Attachments, [], Attachment) of
                notfound ->
                    [Attachment|Attachments];
                A ->
                    A
                end
        end,
    set_value(<<"_attachments">>, {Attachments1}, Doc).
    
%% @spec delete_inline_attachment(Doc::json_obj(), AName::string()) -> json_obj()
%% @doc delete an attachment record in doc. This is different from delete_attachment
%%      change is only applied in Doc object. Save_doc should be save to save changes.
delete_inline_attachment(Doc, AName) when is_list(AName) ->
    delete_inline_attachment(Doc, list_to_binary(AName));
delete_inline_attachment(Doc, AName) when is_binary(AName) ->
    {Props} = Doc,
    case proplists:get_value(<<"_attachments">>, Props) of
        undefined ->
            Doc;
        {Attachments} ->
            case proplists:get_value(AName, Attachments) of
                undefined ->
                    Doc;
                _ ->
                    Attachments1 = proplists:delete(AName, Attachments),
                    set_value(<<"_attachments">>, {Attachments1}, Doc)
                end
        end.
 

%% @private
   
extend1([], JsonObj) ->
    {Props} = JsonObj,
    {lists:reverse(Props)};
extend1([Prop|T], JsonObj) ->
    {Props} = JsonObj,
    extend(T, {[Prop|Props]}).


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
    
get_value1([Key], JsonObj) ->
    get_value(Key, JsonObj);
get_value1([Key|T], JsonObj) ->
    case get_value(Key, JsonObj) of
    List when is_list(List) -> [get_value1(T, X) || X <- List];
    NewObj when is_tuple(NewObj) -> get_value(T, NewObj)
    end.
    

%% @private
set_attachment(Attachments, NewAttachments, Attachment) ->
    set_attachment(Attachments, NewAttachments, Attachment, false).
set_attachment([], Attachments, _Attachment, Found) ->
    case Found of
        true ->
            Attachments;
        false ->
            notfound
        end;
set_attachment([{Name, V}|T], Attachments, Attachment, Found) ->
    {AName, _} = Attachment,
    {Attachment1, Found1} = if
        Name =:= AName, Found =:= false ->
            {Attachment, true};
        true ->
            {{Name, V}, Found}
        end,
    set_attachment(T, [Attachment1|Attachments], Attachment, Found1).