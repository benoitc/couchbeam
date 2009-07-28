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

-module(ecouchdbkit).
-behaviour(application).
-behaviour(gen_server).
-author('Benoît Chesneau <benoitc@e-engura.org').

-export([start/0, start/2, stop/0, stop/1, open_connection/1]).
-export([init/1, handle_call/3,sup_start_link/0]).
-export([handle_cast/2,code_change/3,handle_info/2,terminate/2]).
-export([json_encode/1, json_decode/1]).
-export([server_info/1, all_dbs/1, db_info/2, create_db/2, delete_db/2,
         uuids/0, uuids/1, next_uuid/0, get_doc/3, get_doc/4, 
         save_doc/3, save_doc/4, save_docs/3, save_docs/4, delete_doc/4, 
         query_view/4, query_view/5, is_db/2, all_docs/3]).
         
-include("ecouchdbkit.hrl").
-define(SERVER, ?MODULE).


start() ->
    ecouchdbkit_sup:start_link().
    
stop() ->
    ecouchdbkit_sup:stop().

start(_Type, _StartArgs) ->
    start().
     
stop(_Reason) ->
    stop().
    
sup_start_link() ->
    gen_server:start_link({local, ecouchdbkit}, ecouchdbkit, [], []).
    
open_connection({NodeName, {Host, Port}}) ->
    gen_server:call(ecouchdbkit, {open_connection, {NodeName, {Host, Port}}}).
    
json_encode(V) ->
    ecouchdbkit_mochijson2:encode(V).
    
json_decode(V) ->
    ecouchdbkit_mochijson2:decode(V).
    
    
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
    
is_db(NodeName, DbName) ->
    AllDbs = all_dbs(NodeName),
    lists:member(?l2b(DbName), AllDbs).

%% document operations

get_doc(NodeName, DbName, DocId) ->
    get_doc(NodeName, DbName, DocId, nil).
get_doc(NodeName, DbName, DocId, Rev) ->
    Path = "/" ++ DbName ++ "/" ++ DocId,
    Resp = case Rev of
        nil -> 
            make_request(NodeName, 'GET', Path, []);
        _Rev ->
            make_request(NodeName, 'GET', Path, [], [{"rev", Rev}])
        end,
    do_reply(Resp).
    
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
    Docs1 = [maybe_docid(Doc) || Doc <- Docs],
    JsonObj = case proplists:get_value(all_or_nothing, Opts, false) of
    true -> {[{<< "all_or_nothing">>, true}, {<<"docs">>, Docs1}]};
    false -> {[{<<"docs">>, Docs1}]}
    end,    
    Body = ecouchdbkit:json_encode(JsonObj),
    Path = "/" ++ DbName ++ "/_bulk_docs",
    Resp = make_request(NodeName, 'POST', Path, Body, [], []),
    do_reply(Resp).
    
delete_doc(NodeName, DbName, DocId, Rev) ->
    Path = io_lib:format("/~s/~s?rev=~s", [DbName, DocId, Rev]),
    Resp = make_request(NodeName, 'DELETE', Path, []),
    do_reply(Resp).
    
all_docs(NodeName, DbName, Params) ->
    Path = io_lib:format("/~s/_all_docs", [DbName]),
    fetch_view(NodeName, Path, Params).
    
query_view(NodeName, DbName, DName, ViewName) ->
    query_view(NodeName, DbName, DName, ViewName, []).
    
query_view(NodeName, DbName, DName, ViewName, Params) ->
    Path = io_lib:format("/~s/_design/~s/_view/~s", [DbName, DName, ViewName]),
    fetch_view(NodeName, Path, Params).
    
fetch_view(NodeName, Path, Params) ->
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
    {error, Reason} -> throw(Reason);
    {json, {[{<<"ok">>, true}]}} -> ok;
    {json, {[{<<"ok">>, true}|Res]}} -> {ok, Res};
    {json, {Obj}} -> Obj;
    {json, Obj} -> Obj;
    Other -> Other
    end.
    
make_request(NodeName, Method, Path, Headers) ->
    make_request(NodeName, Method, Path, nil, Headers, []).
    
make_request(NodeName, Method, Path, Headers, Params) ->
    make_request(NodeName, Method, Path, nil, Headers, Params).
    
make_request(NodeName, Method, Path, Body, Headers, Params) ->
    case gen_server:call(ecouchdbkit, {get, NodeName}) of
    {error, Reason} -> 
        {error, Reason};
    Pid -> 
        gen_server:call(Pid, {request, Method, Path, Body, Headers, Params})
    end.
    
    
init([]) ->
    Tid = ets:new(ecouchdbkit_uuids, [public, ordered_set]),
    State = #ecouchdbkit_srv{
        ets_tid=Tid
    },
    {ok, State}.
    

handle_call({get, NodeName}, _From, #ecouchdbkit_srv{nodes=Nodes}=State) ->
    AllNodes = supervisor:which_children(ecouchdbkit_nodes),
    ErrorMsg = lists:flatten(
        io_lib:format("No couchdb node configured for ~p.", [NodeName])),
    R = case find_node(AllNodes, NodeName) of
    false ->
        case proplists:get_value(NodeName, Nodes) of
        undefined ->
            {error, {unknown_couchdb_node, ?l2b(ErrorMsg)}};
        Node ->
            #couchdb_node{
                host=Host,
                port=Port
            } = Node,
            {Pid, _} = open_connection1({NodeName, {Host, Port}}, Nodes),
            Pid
        end;
    Pid -> Pid
    end,
    {reply, R, State};
    
handle_call({open_connection, {NodeName, {Host, Port}}}, _From, #ecouchdbkit_srv{nodes=Nodes}=State) ->
    {_, Nodes1} = open_connection1({NodeName, {Host, Port}}, Nodes),
    {reply, ok, State#ecouchdbkit_srv{nodes=Nodes1}};
    
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

terminate(_Reason, _State) ->
    ok.
    
handle_cast(Msg, _Server) ->
    exit({unknown_cast_message, Msg}).
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_info(Info, _Server) ->
    exit({unknown_message, Info}).
    
%% Internal API
open_connection1({NodeName, {Host, Port}}, Nodes) ->
    NodeName1 = nodename(NodeName),
    Client = {NodeName1,
        {ecouchdbkit_client, start_link, [{Host, Port}]},
        permanent,
        brutal_kill,
        worker,
        [ecouchdbkit_client]},
    Pid = case supervisor:start_child(ecouchdbkit_nodes, Client) of
    {ok, Pid1} -> Pid1;
    {error, {already_started, _}} ->
        %% terminate child and create a new one with settings
        supervisor:terminate_child(ecouchdbkit_nodes, NodeName1),
        supervisor:delete_child(ecouchdbkit_nodes, NodeName1),
        Nodes1 = proplists:delete(NodeName1, Nodes),
        open_connection1({NodeName, {Host, Port}}, Nodes1);
    {error, already_present} ->
        case supervisor:start_child(ecouchdbkit_nodes, Client) of
        {ok, Pid1} -> Pid1;
        {error, already_present} ->
            case supervisor:restart_child(ecouchdbkit_nodes, NodeName1) of
            {ok, Pid1} -> Pid1;
            {error, running} ->
                {error, {already_started, Pid1}} =
                    supervisor:start_child(ecouchdbkit_nodes, Client),
                Pid1
            end
        end
    end,
    Node = {NodeName1, #couchdb_node{host=Host, port=Port}},
    Nodes2 = [Node|proplists:delete(NodeName1, Nodes)],
    {Pid, Nodes2}.
    
new_uuid(Tid) ->
    [Id|Uuids] = ecouchdbkit_util:generate_uuids(1000),
    ets:insert(Tid, {uuids, Uuids}),
    Id.
    
maybe_docid({DocProps}) ->
    case proplists:get_value(<<"_id">>, DocProps) of
        undefined ->
            DocId = couchdbkit_util:new_uuid(),
            {[{<<"_id">>, DocId}|DocProps]};
        _DocId ->
            {DocProps}
    end.
    
nodename(N) when is_binary(N) ->
    list_to_atom(?b2l(N));
nodename(N) when is_list(N) ->
    list_to_atom(N);
nodename(N) when is_atom(N) ->
    N.
    
find_node([], _Name) ->
    false;
find_node([Node|R], Name) ->
    {Id, Pid, _, _} = Node,
    if
    Id =:= Name -> 
        Pid;
    true ->
        find_node(R, Name)
    end.