%% @author Benoît Chesneau <benoitc@e-engura.org>
%% @copyright 2009 Benoît Chesneau.
%%
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(couchbeam_db).
-author('Benoît Chesneau <benoitc@e-engura.org').

-behaviour(gen_server).

-include("couchbeam.hrl").

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2]).
-export([db_info/1, open_doc/2, open_doc/3, save_doc/2, save_doc/3, save_docs/2,
         save_docs/3, delete_doc/2, delete_docs/2, delete_docs/3]).
-export([query_view/3, all_docs/2, all_docs_by_seq/2]).
-export([fetch_attachment/3, fetch_attachment/4, put_attachment/5, put_attachment/6, 
         delete_attachment/3]).


%% @type node_info() = {Host::string(), Port::int()}
%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()
%% @type json_string() = atom | binary()
%% @type json_number() = integer() | float()
%% @type json_array() = [json_term()]
%% @type json_object() = {struct, [{json_string(), json_term()}]}
%% @type json_term() = json_string() | json_number() | json_array() |
%%                     json_object()


%% @spec db_info(Db::pid()) -> list()
%% @doc fetch information of Database
db_info(Db) ->
    gen_server:call(Db, info, infinity).
    
    
    
%%---------------------------------------------------------------------------
%% Manage docs
%%---------------------------------------------------------------------------

%% @spec open_doc(Db::pid(), DocId::string()) -> json_object()
%% @doc open a doc with DocId
open_doc(Db, DocId) ->
    open_doc(Db, DocId, []).

open_doc(Db, DocId, Params) ->
    gen_server:call(Db, {open_doc, DocId, Params}, infinity).
   
%% @spec save_doc(Db::pid(), Doc::json_object()) -> json_object() 
%% @doc save a do with DocId. 
save_doc(Db, Doc) ->
    save_doc(Db, Doc, []).

save_doc(Db, Doc, Params) ->
    gen_server:call(Db, {save_doc, Doc, Params}, infinity). 
   
%% @spec save_docs(Db::pid(), Docs::json_array()) -> json_object() 
%% @doc bulk update
save_docs(Db, Docs) ->
    save_docs(Db, Docs, []).

%% @spec save_docs(Db::pid(), Docs::json_array(), opts: lists()) -> json_object() 
%% @doc bulk update with options, currently support only all_or_nothing.    
save_docs(Db, Docs, Params) ->
    gen_server:call(Db, {save_docs, Docs, Params}, infinity). 
    
%% @spec delete_doc(Db::pid(), Doc::json_object()) -> json_object() 
%% @doc delete a document
delete_doc(Db, {DocProps}) ->
    Doc1 = {[{<<"_deleted">>, true}|DocProps]},
    save_doc(Db, Doc1).

delete_docs(Db, Docs) ->
    delete_docs(Db, Docs, []).
    
delete_docs(Db, Docs, Opts) ->
    Docs1 = lists:map(fun({DocProps})->
        {[{<<"_deleted">>, true}|DocProps]}
        end, Docs),
    save_docs(Db, Docs1, Opts).
  

%%---------------------------------------------------------------------------
%% View docs
%%---------------------------------------------------------------------------  
    
%% @spec query_view(Db::pid(), 
%%                  ViewName::view_name(),
%%                  Params::view_params()) -> json_object()
%% @type view_params() = proplist()
%% @doc query a view and return results depending on params
    
query_view(Db, Vname, Params) ->
    gen_server:call(Db, {query_view, Vname, Params}, infinity). 

%% @spec all_docs(Db::pid(), Params::list()) -> json_object()
%% @doc This method has the same behavior as a view. Return all docs
all_docs(Db, Params) ->
    query_view(Db, '_all_docs', Params).
    
%% @spec all_docs_by_seq(Db::pid(), Params::list()) -> json_object()
%% @deprecated  This feature don't exist anymore in couchdb 0.11, use suscribe() instead.
%% @doc This method has the same behavior as a view. 
%% Return an updated list of all documents.
all_docs_by_seq(Db, Params) ->
    query_view(Db, '_all_docs_by_seq', Params).
   
   
%%---------------------------------------------------------------------------
%% Manage attachments 
%%---------------------------------------------------------------------------

 
%% @spec fetch_attachment(Db::pid(), Doc::json_obj(), 
%%                  AName::string()) -> iolist()
%% @doc fetch attachment
fetch_attachment(Db, Doc, AName) ->
    fetch_attachment(Db, Doc, AName, []).

fetch_attachment(Db, Doc, AName, Opts) ->
    gen_server:call(Db, {fetch_attachment, Doc, AName, Opts}, infinity). 

%% @spec put_attachment(Db::pid(), Doc::json_obj(),
%%      Content::attachment_content(), AName::string(), Length::string()) -> json_obj()
%% @type attachment_content() = string() |binary() | fun_arity_0() | {fun_arity_1(), initial_state()}
%% @doc put attachment attachment, It will try to guess mimetype
put_attachment(Db, Doc, Content, AName, Length) ->
    ContentType = couchbeam_util:guess_mime(AName),
    put_attachment(Db, Doc, Content, AName, Length, ContentType).
    
%% @spec put_attachment(Db::pid(), Doc::json_obj(),
%%      Content::attachment_content(), AName::string(), Length::string(), ContentType::string()) -> json_obj()
%% @doc put attachment attachment with ContentType fixed.
put_attachment(Db, Doc, Content, AName, Length, ContentType) ->
    gen_server:call(Db, {put_attachment, Doc, Content, AName, Length, ContentType}, infinity). 
    
%% @spec delete_attachment(Db::pid(), Doc::json_obj(),
%%      AName::string()) -> json_obj()
%% @doc delete attachment
delete_attachment(Db, Doc, AName) ->
    gen_server:call(Db, {delete_attachment, Doc, AName}, infinity). 


%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------
%% @private

init({DbName, #server_state{couchdb=C, prefix=Prefix} = ServerState}) ->
    State = #db{server  = ServerState,
                couchdb = C,
                base    = Prefix ++ DbName},
    {ok, State}.
    
handle_call(info, _From, #db{couchdb=C, base=Base} = State) ->
    Infos = case couchbeam_resource:get(C, Base, [], [], []) of
        {ok, {Infos1}} -> Infos1;
        {error, Reason} -> Reason
    end,
    {reply, Infos, State};
    
handle_call({open_doc, DocId, Params}, _From, #db{couchdb=C, base=Base} = State) ->
    Path = Base ++ "/" ++ DocId,
    Doc = case couchbeam_resource:get(C, Path, [], Params, []) of
        {ok, Doc1} -> Doc1;
        {error, Reason} -> Reason
    end,
    {ok, Doc, State};
    
handle_call({save_doc, Doc, Params}, _From, #db{server=ServerState, couchdb=C, 
                                                base=Base} = State) ->
    {Props} = Doc,
    DocId = case proplists:get_value(<<"_id">>, Props) of
        undefined ->
            #server_state{uuids_pid=UuidsPid} = ServerState,
            couchbeam_uuids:next_uuid(UuidsPid);
        Id1 -> Id1
    end,
    Path = Base ++ "/" ++ DocId,
    Body = couchbeam:json_encode(Doc),
    Resp = case couchbeam_resource:put(C, Path, Body, [], Params, []) of
        {ok, {Props1}} ->
            NewRev = proplists:get_value(Props1, <<"rev">>),
            DocId1 = proplists:get_value(Props1, <<"id">>),
            Doc1 = couchbeam_doc:extend([{<<"_id">>, DocId1}, {<<"_rev">>, NewRev}]),
            Doc1;
        {error, Reason} ->
            Reason
    end,
    {reply, Resp, State};
            
handle_call({save_docs, Docs, Opts}, _From, #db{couchdb=C,base=Base} = State) ->
    Docs1 = [maybe_docid(State, Doc) || Doc <- Docs],
    JsonObj = case proplists:get_value(all_or_nothing, Opts, false) of
        true -> {[{<< "all_or_nothing">>, true}, {<<"docs">>, Docs1}]};
        false -> {[{<<"docs">>, Docs1}]}
    end,
    Body = couchbeam:json_encode(JsonObj),
    Path = Base ++ "/_bulk_docs",
    Res = case couchbeam_resource:post(C, Path, [], [], Body, []) of
        {ok, Results} ->
            % TODO: we could aggregate resulst here, maybe an option?
            Results;
        {error, Reason} -> Reason
    end,
    {reply, Res, State};
    
handle_call({query_view, Vname, Params}, _From, State) ->
    ViewPid = gen_server:start_link(couchbeam_view, {Vname, Params, State}),
    {reply, ViewPid, State};
     
handle_call({fetch_attachment, DocId, AName, Opts}, _From, #db{couchdb=C, base=Base}=State) ->
    Path = io_lib:format("~s/~s/~s", [Base, DocId, AName]),
    Resp = case couchbeam_resource:get(C, Path, [], [], Opts) of
        {error, Reason} -> Reason;
        R -> R
    end,
    {reply, Resp, State};

handle_call({put_attachment, Doc, Content, AName, Length, ContentType}, _From, 
            #db{couchdb=C, base=Base}=State) ->
    {DocId, Rev, IsJson} = case Doc of
        {Id, Rev1} -> {Id, Rev1, false};
        _ ->
            DocId1 = couchbeam_doc:get_value(<<"_id">>, Doc),
            Rev2 = couchbeam_doc:get_value(<<"_rev">>, Doc),
            {DocId1, Rev2, true}
    end,
    Headers = [{"Content-Length", couchbeam_util:val(Length)}, {"Content-Type", ContentType}],
    Path = io_lib:format("~s/~s/~s", [Base, DocId, AName]),
    Resp = case couchbeam_resource:put(C, Path, Headers, [{"rev", Rev}], Content, []) of
        {error, Reason} -> Reason;
        {ok, R} when (IsJson =:= true) ->
             {Props} = R,
             NewRev = proplists:get_value(Props, <<"rev">>),
             DocId2 = proplists:get_value(Props, <<"id">>),
             Doc1 = couchbeam_doc:extend([{<<"_id">>, DocId2}, {<<"_rev">>, NewRev}]),
             Doc1;
        {ok, R} ->  R
    end,
    {reply, Resp, State};
    
handle_call({delete_attachment, Doc, AName}, _From, #db{couchdb=C, base=Base}=State) ->
    {DocId, Rev, IsJson} = case Doc of
        {Id, Rev1} -> {Id, Rev1, false};
        _ ->
            DocId1 = couchbeam_doc:get_value(<<"_id">>, Doc),
            Rev2 = couchbeam_doc:get_value(<<"_rev">>, Doc),
            {DocId1, Rev2, true}
    end,
    Path = io_lib:format("~s/~s/~s", [Base, DocId, AName]),
    Resp = case couchbeam_resource:delete(C, Path, [], [{"rev", Rev}], []) of
        {error, Reason} -> Reason;
        {ok, R} when (IsJson =:= true) ->
             {Props} = R,
             NewRev = proplists:get_value(Props, <<"rev">>),
             DocId2 = proplists:get_value(Props, <<"id">>),
             Doc1 = couchbeam_doc:extend([{<<"_id">>, DocId2}, {<<"_rev">>, NewRev}]),
             Doc1;
        {ok, R} ->  R
    end,
    {reply, Resp, State}.
    
handle_cast(_Msg, State) ->
    {no_reply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%% @private

maybe_docid(#db{server=ServerState}, {DocProps}) ->
    #server_state{uuids_pid=UuidsPid} = ServerState,
    case proplists:get_value(<<"_id">>, DocProps) of
        undefined ->
            DocId = couchbeam_uuids:next_uuid(UuidsPid),
            {[{<<"_id">>, DocId}|DocProps]};
        _DocId ->
            {DocProps}
    end.
    