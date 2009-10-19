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

-module(couchbeam_db).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-behaviour(gen_server).

-include("couchbeam.hrl").

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2]).
-export([info/1, open_doc/2, open_doc/3, save_doc/2, save_doc/3, save_docs/2,
         save_docs/3, delete_doc/2, delete_docs/2, delete_docs/3]).
-export([query_view/3, all_docs/2, all_docs_by_seq/2]).
-export([fetch_attachment/3, fetch_attachment/4, put_attachment/5, put_attachment/6, 
         delete_attachment/3]).
-export([open/2, close/2, create/2, delete/2, open_or_create/2]).
-export([suscribe/2, suscribe/3]).

%% @type node_info() = {Host::string(), Port::int()}
%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()
%% @type json_string() = atom | binary()
%% @type json_number() = integer() | float()
%% @type json_array() = [json_term()]
%% @type json_object() = {struct, [{json_string(), json_term()}]}
%% @type json_term() = json_string() | json_number() | json_array() |
%%                     json_object()


%% @spec info(Db::pid()) -> list()
%% @doc fetch information of Database
info(Db) ->
    gen_server:call(db_pid(Db), info, infinity).

open(ConnectionPid, DbName) ->
    couchbeam_server:open_db(ConnectionPid, DbName).

close(ConnectionPid, Db) ->
    couchbeam_server:close_db(ConnectionPid, db_pid(Db)).
    
create(ConnectionPid, DbName) ->
    couchbeam_server:create_db(ConnectionPid, DbName).
    
delete(ConnectionPid, DbName) ->
    couchbeam_server:delete_db(ConnectionPid, DbName).
    
open_or_create(ConnectionPid, DbName) ->
    couchbeam_server:open_or_create_db(ConnectionPid, DbName).
    
%%---------------------------------------------------------------------------
%% Manage docs
%%---------------------------------------------------------------------------

%% @spec open_doc(Db::pid(), DocId::string()) -> json_object()
%% @doc open a doc with DocId
open_doc(Db, DocId) ->
    open_doc(Db, DocId, []).

open_doc(Db, DocId, Params) ->
    gen_server:call(db_pid(Db), {open_doc, DocId, Params}, infinity).
   
%% @spec save_doc(Db::pid(), Doc::json_object()) -> json_object() 
%% @doc save a do with DocId. 
save_doc(Db, Doc) ->
    save_doc(Db, Doc, []).

save_doc(Db, Doc, Params) ->
    gen_server:call(db_pid(Db), {save_doc, Doc, Params}, infinity). 
   
%% @spec save_docs(Db::pid(), Docs::json_array()) -> json_object() 
%% @doc bulk update
save_docs(Db, Docs) ->
    save_docs(Db, Docs, []).

%% @spec save_docs(Db::pid(), Docs::json_array(), opts: lists()) -> json_object() 
%% @doc bulk update with options, currently support only all_or_nothing.    
save_docs(Db, Docs, Params) ->
    gen_server:call(db_pid(Db), {save_docs, Docs, Params}, infinity). 
    
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
%% suscribe to update notifications
%%---------------------------------------------------------------------------  
    
%% @spec suscribe(Db::pid(), Consumer::pid()) -> pid() 
%% @doc suscribe to db changes
suscribe(Db, Consumer) ->
    suscribe(Db, Consumer, []).
    
%% @spec suscribe(Db::pid(), Consumer::pid(), Options::ChangeOptions()) -> pid()
%% @type ChangeOptions [ChangeOption]
%%       ChangeOption = {heartbeat, integer | string} |
%%                      {timeout, integer()}}
%% @doc suscribe to db changes, wait for changes heartbeat 
suscribe(Db, Consumer, Options) ->
    gen_server:call(db_pid(Db), {suscribe_changes, Consumer, Options}, infinity).
  

%%---------------------------------------------------------------------------
%% View docs
%%---------------------------------------------------------------------------  
    
%% @spec query_view(Db::pid(), 
%%                  ViewName::view_name(),
%%                  Params::view_params()) -> json_object()
%% @type view_params() = proplist()
%% @doc query a view and return results depending on params
    
query_view(Db, Vname, Params) ->
    gen_server:call(db_pid(Db), {query_view, Vname, Params}, infinity). 

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
    gen_server:call(db_pid(Db), {fetch_attachment, Doc, AName, Opts}, infinity). 

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
    gen_server:call(db_pid(Db), {put_attachment, Doc, Content, AName, Length, ContentType}, infinity). 
    
%% @spec delete_attachment(Db::pid(), Doc::json_obj(),
%%      AName::string()) -> json_obj()
%% @doc delete attachment
delete_attachment(Db, Doc, AName) ->
    gen_server:call(db_pid(Db), {delete_attachment, Doc, AName}, infinity). 


%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------
%% @private

init({DbName, #server_state{couchdb=C, prefix=Prefix} = ServerState}) ->
    State = #db{name    = DbName,
                server  = ServerState,
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
    {reply, Doc, State};
    
handle_call({save_doc, Doc, Params}, _From, #db{server=ServerState, couchdb=C, 
                                                base=Base} = State) ->
    {Props} = Doc,
    DocId = case proplists:get_value(<<"_id">>, Props) of
        undefined ->
            #server_state{uuids_pid=UuidsPid} = ServerState,
            couchbeam_uuids:next_uuid(UuidsPid);
        Id1 when is_list(Id1) -> Id1;
        Id1 -> ?b2l(Id1)
    end,
    Path = Base ++ "/" ++ DocId,
    Body = couchbeam:json_encode(Doc),
    Resp = case couchbeam_resource:put(C, Path, [], Params, Body, []) of
        {ok, {Props1}} ->
            NewRev = proplists:get_value(<<"rev">>, Props1),
            DocId1 = proplists:get_value(<<"id">>, Props1),
            Doc1 = couchbeam_doc:set_value(<<"_id">>, DocId1, Doc),
            Doc2 = couchbeam_doc:set_value(<<"_rev">>, NewRev, Doc1),
            Doc2;
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
    {ok, ViewPid} = gen_server:start_link(couchbeam_view, {Vname, Params, State}, []),
    {reply, ViewPid, State};
     
handle_call({fetch_attachment, DocId, AName, Opts}, _From, #db{couchdb=C, base=Base}=State) ->
    Path = io_lib:format("~s/~s/~s", [Base, DocId, AName]),
    Resp = case couchbeam_resource:get(C, Path, [], [], Opts) of
        {error, Reason} -> Reason;
        {ok, R} -> R
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
             NewRev = proplists:get_value(<<"rev">>, Props),
             DocId2 = proplists:get_value(<<"id">>, Props),
             Doc1 = couchbeam_doc:set_value(<<"_id">>, DocId2, Doc),
             Doc2 = couchbeam_doc:set_value(<<"_rev">>, NewRev, Doc1),
             Doc2;
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
             NewRev = proplists:get_value(<<"rev">>, Props),
             DocId2 = proplists:get_value(<<"id">>, Props),
             Doc1 = couchbeam_doc:set_value(<<"_id">>, DocId2, Doc),
             Doc2 = couchbeam_doc:set_value(<<"_rev">>, NewRev, Doc1),
             Doc2;
        {ok, R} ->  R
    end,
    {reply, Resp, State};
    
handle_call({suscribe_changes, Consumer, Options}, _From, #db{base=Base} = State) ->
    {Timeout, Options1} = get_option(timeout, Options),
    {HeartBeat, Options2} = get_option(heartbeat, Options1),
    
    ExtraParams = case HeartBeat of
        undefined ->
            case Timeout of
                undefined -> [];
                T when is_integer(T) -> 
                    [{timeout,  integer_to_list(T)}];
                T ->
                    [{timeout, T}]
            end;
        H when is_integer(H) ->
            [{heartbeat, integer_to_list(H)}];
        H ->
            [{heartbeat, H}]
    end,
    ExtraParams1 = ExtraParams ++ Options2,

    Path0 = Base ++ "/_changes?feed=continuous",
    Path = case ExtraParams1 of
        [] -> Path0;
        Extra -> Path0 ++ "&" ++ couchbeam_resource:encode_query(Extra)
    end,
    ChangeState = #change{db=State, path=Path, consumer_pid=Consumer},
    Pid = spawn_link(fun() -> suscribe_changes(ChangeState) end),
    {reply, Pid, State}.
      
handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info({'EXIT', _Pid, _Reason}, State) ->
    {stop, State};
    
handle_info(Msg, State) ->
    io:format("Bad message received for db ~s: ~p", [State#db.name, Msg]),
    exit({error, Msg}).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%% @private
suscribe_changes(#change{consumer_pid=ConsumerPid}=ChangeState) ->
    try
        do_suscribe(ChangeState)
    catch
        Reason ->
            ConsumerPid ! Reason;
        exit:Reason ->
            ConsumerPid ! {'EXIT', Reason};
        error:Reason ->
            ConsumerPid ! {'error', Reason}
    end.

do_suscribe(#change{db=DbState, path=Path, consumer_pid=ConsumerPid}) ->
    #db{couchdb=CouchdbState} = DbState,
    #couchdb_params{host=Host, port=Port, ssl=Ssl, timeout=Timeout}=CouchdbState,
    Headers = [{"Accept", "application/json"}],
    Options = [{partial_download, [{window_size, infinity}]}],
    case lhttpc:request(Host, Port, Ssl, Path, 'GET', Headers, <<>>, Timeout, Options) of
        {ok, {{_, _}, _, ResponseBody}} ->
            case ResponseBody of
                Body when is_pid(Body) ->
                    ConsumerPid ! {body_pid, ResponseBody},
                    InitialState = lhttpc:get_body_part(ResponseBody),
                    send_changes(InitialState, ConsumerPid, ResponseBody);
                _Body ->
                    ConsumerPid ! {body_done, ResponseBody}
            end;
        {error, Reason} ->
            ConsumerPid ! {error, Reason}
    end.
    
send_changes({ok, {http_eob, _Trailers}}, ConsumerPid, _Pid) ->
    ConsumerPid ! body_done,
    ok;
send_changes({ok, [<<"\n">>]}, ConsumerPid, Pid) ->    
    NextState = lhttpc:get_body_part(Pid),
    send_changes(NextState, ConsumerPid, Pid);
send_changes({ok, Bin}, ConsumerPid, Pid) ->
    Lines = decode_lines(string:tokens(Bin, "\n"), []),
    ConsumerPid ! {change, Lines},
    NextState = lhttpc:get_body_part(Pid),
    send_changes(NextState, ConsumerPid, Pid).
    
decode_lines([], Acc) ->
    lists:flatten(lists:reverse(Acc));
decode_lines([Line|Rest], Acc) ->
    Line1 = couchbeam:json_decode(list_to_binary(Line)),
    decode_lines(Rest, [Line1, Acc]).

maybe_docid(#db{server=ServerState}, {DocProps}) ->
    #server_state{uuids_pid=UuidsPid} = ServerState,
    case proplists:get_value(<<"_id">>, DocProps) of
        undefined ->
            DocId = couchbeam_uuids:next_uuid(UuidsPid),
            {[{<<"_id">>, DocId}|DocProps]};
        _DocId ->
            {DocProps}
    end.
    

db_pid(Db) when is_pid(Db) ->
    Db;
db_pid(Db) ->
    couchbeam_manager:get_db(Db).
    
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
