%% @author Benoît Chesneau <benoitc@e-engura.org>
%% @copyright 2009 Benoît Chesneau.
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%      http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.

-module(ecouchdbkit).
-behaviour(application).
-behaviour(gen_server).
-author('Benoît Chesneau <benoitc@e-engura.org').

-export([ start/2, stop/1, open_connection/1]).
-export([init/1, handle_call/3,sup_start_link/0]).
-export([handle_cast/2,code_change/3,handle_info/2,terminate/2]).
-export([json_encode/1, json_decode/1]).
-export([server_info/1, all_dbs/1, db_info/2, create_db/2, delete_db/2,
         uuids/0, uuids/1, next_uuid/0,  save_doc/3, save_doc/4, save_docs/3,
         save_docs/4, delete_doc/4, query_view/4, query_view/5]).
         
-include("ecouchdbkit.hrl").


start(_Type, _InitArgs) ->
    ecouchdbkit_sup:start_link().
     
stop(_Reason) ->
    ecouchdbkit_sup:stop().
    
sup_start_link() ->
    gen_server:start_link({local, ecouchdbkit}, ecouchdbkit, [], []).
    
open_connection({NodeName, {Host, Port}}) ->
    gen_server:call(ecouchdbkit, {config, {NodeName, {Host, Port}}}).
    
json_encode(V) ->
    couchdb_mochijson2:encode(V).
    
json_decode(V) ->
    couchdb_mochijson2:decode(V).
    
    
%% server operations 
server_info(NodeName) ->
    Resp = make_request(NodeName, 'GET', "/", []),
    do_reply(Resp).
    
uuids() ->
    gen_server:call(ecouchdbkit, uuids).
    
uuids(Count) ->
    gen_server:call(ecouchdbkit, {uuids, Count}).
    
next_uuid() ->
    gen_server:call(ecouchdbkit, next_uuid).
    
all_dbs(NodeName) ->
    Resp = make_request(NodeName, 'GET', "/_all_dbs", []),
    do_reply(Resp).
    
%% db operations   
    
db_info(NodeName, DbName) ->
    Resp = make_request(NodeName, 'GET', "/" ++ DbName, []),
    do_reply(Resp).
 
create_db(NodeName, DbName) ->
    Resp = make_request(NodeName, 'PUT', "/" ++ DbName, []),
    do_reply(Resp).
    
delete_db(NodeName, DbName) ->
    Resp = make_request(NodeName, 'DELETE', "/" ++ DbName, []),
    do_reply(Resp).

%% document operations

save_doc(NodeName, DbName, Doc) ->
    {Props} = Doc,
    DocId = case proplists:get_value(<<"_id">>, Props) of
    undefined -> next_uuid();
    Id1 -> Id1
    end,
    save_doc(NodeName, DbName, ?b2l(DocId), Doc).
    
save_doc(NodeName, DbName, DocId, Doc) ->
    Path = "/" ++ DbName ++ "/" ++ DocId,
    Body = ecouchdbkit:json_encode(Doc),
    Resp = make_request(NodeName, 'PUT', Path, Body, [], []),
    do_reply(Resp).
    
save_docs(NodeName, DbName, Docs) ->
    save_docs(NodeName, DbName, Docs, []).
        
save_docs(NodeName, DbName, Docs, Opts) ->
    JsonObj = case proplists:get_value(all_or_nothing, Opts, false) of
    true -> [{<< "all_or_nothing">>, true}, {<<"docs">>, Docs}];
    false -> {[{<<"docs">>, Docs}]}
    end,    
    Body = ecouchdbkit:json_encode(JsonObj),
    Path = "/" ++ DbName ++ "/_bulk_docs",
    Resp = make_request(NodeName, 'POST', Path, Body, [], []),
    do_reply(Resp).
    
delete_doc(NodeName, DbName, DocId, Rev) ->
    Path = io_lib:format("/~s/~s?rev=~s", [DbName, DocId, Rev]),
    Resp = make_request(NodeName, 'DELETE', Path, []),
    do_reply(Resp).
    
query_view(NodeName, DbName, DName, ViewName) ->
    query_view(NodeName, DbName, DName, ViewName, []).
    
query_view(NodeName, DbName, DName, ViewName, Params) ->
    Path = io_lib:format("/~s/_design/~s/_view/~s", [DbName, DName, ViewName]),
    Resp = case proplists:get_value("keys", Params) of
        undefined -> 
            make_request(NodeName, 'GET', Path, [], Params);
        Keys ->
            Params1 = proplists:delete("keys", Params),
            Body = ecouchdbkit:json_encode({struct, {<<"keys">>, Keys}}),
            make_request(NodeName, 'POST', Path, Body, [], Params1)
        end,
    do_reply(Resp).
    
do_reply(Resp) ->
    case Resp of
    {json, {[{<<"ok">>, true}]}} -> ok;
    {json, {[{<<"ok">>, true}|Res]}} -> {ok, Res};
    {json, {Obj}} -> Obj;
    {json, Obj} -> Obj;
    Err -> {error, Err}
    end.    
make_request(NodeName, Method, Path, Headers) ->
    make_request(NodeName, Method, Path, nil, Headers, []).
    
make_request(NodeName, Method, Path, Headers, Params) ->
    make_request(NodeName, Method, Path, nil, Headers, Params).
    
make_request(NodeName, Method, Path, Body, Headers, Params) ->
    Pid = gen_server:call(ecouchdbkit, {get, NodeName}),
    gen_server:call(Pid, {request, Method, Path, Body, Headers, Params}).  
    
    
init([]) ->
    Tid = ets:new(ecouchdbkit, [public, ordered_set]),
    NodesTid = ets:new(ecouchdbkit_nodes, [set, private]),
    
    DefaultClient = {default,
        {ecouchdbkit_client, start_link, {"127.0.0.1", 5984}},
        permanent,
        brutal_kill,
        worker,
        [ecouchdbkit_client]
    },
    
    %%case supervisor:start_child(ecouchdbkit_sup, DefaultClient) of
    %%{ok, Pid} ->
    %%    io:format("{~p, ~p}~n", [default, Pid]),
    %%    ets:insert(NodesTid, {default, Pid});
    %%_ -> ok
    %%end,
    
    State = #ecouchdbkit_srv{
        ets_tid=Tid,
        nodes_tid=NodesTid
    },
    {ok, State}.
    

handle_call({get, NodeName}, _From, #ecouchdbkit_srv{nodes_tid=NodesTid} = State) ->
    case ets:lookup(NodesTid, NodeName) of
    [] ->
        Msg = lists:flatten(
            io_lib:format("No couchdb node configured for ~p.", [NodeName])),
        {reply, {error, {unknown_couchdb_node, ?l2b(Msg)}}, NodesTid};
    [{NodeName, Pid}] ->
         {reply, Pid, State}
    end;
    
handle_call({config, {NodeName, {Host, Port}}}, _From, #ecouchdbkit_srv{nodes_tid=NodesTid} = State) ->
    NodeName1 = list_to_atom(NodeName),
    case ets:lookup(NodesTid, NodeName) of
    [{NodeName1, Pid}] ->
        ecouchdbkit_client:stop(Pid);
    [] -> ok
    end,
    {ok, Pid1} = ecouchdbkit_client:start_link({Host, Port}),
    ets:insert(NodesTid, {NodeName1, Pid1}),
    {reply, ok, State};
        
handle_call(next_uuid, _From, #ecouchdbkit_srv{ets_tid=Tid}=State) ->
    R = case ets:lookup(Tid, uuids) of
    [] -> new_uuid(Tid);
    [{uuids, []}] -> new_uuid(Tid);
    [{uuids, [Id2|Uuids2]}] ->
        ets:insert(Tid, {uuids, Uuids2}),
        Id2
    end,
    {reply, R, State};
    
handle_call(uuids, _From, State) ->
    NewUuid = couchdbkit_util:new_uuid(),
    {reply, NewUuid, State};

handle_call({uuids, Count}, _From, State) ->
    UUIDs = [ couch_util:new_uuid() || _ <- lists:seq(1,Count)],
    {reply, UUIDs, State}.

terminate(_Reason, #ecouchdbkit_srv{nodes_tid=NodesTid}) ->
    ets:foldl(fun({_NodeName, Pid}, nil) ->
        ecouchdbkit_client:stop(Pid),
        nil
    end, nil, NodesTid),
    ok.
    
handle_cast(Msg, _Server) ->
    exit({unknown_cast_message, Msg}).
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_info({'EXIT', Pid, _Reason}, #ecouchdbkit_srv{ets_tid=Tid, nodes_tid=NodesTid}=State) ->
    true = ets:delete(Tid),
    ets:match_delete(NodesTid, {'_', Pid}),
    {noreply, State};
    
handle_info(Info, _Server) ->
    exit({unknown_message, Info}).
    
%% Internal API
new_uuid(Tid) ->
    [Id|Uuids] = ecouchdbkit_util:generate_uuids(1000),
    ets:insert(Tid, {uuids, Uuids}),
    Id.