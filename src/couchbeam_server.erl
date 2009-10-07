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

-module(couchbeam_server).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-behaviour(gen_server).

-include("couchbeam.hrl").



-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2]).
-export([start_connection/0, start_connection/1, start_connection_link/0,
         start_connection_link/1]).
-export([info/1, create_db/2, open_db/2, open_or_create_db/2, delete_db/2,
         all_dbs/1, is_db/2, close_db/2]).
         
 
start_connection() -> start_connection(#couchdb_params{}).

start_connection(Params) -> start_connection_internal(Params, false).

start_connection_link() -> start_connection_link(#couchdb_params{}).

start_connection_link(Params) -> start_connection_internal(Params, true).        

start_connection_internal(#couchdb_params{prefix=Prefix,name=Name} = CouchdbParams,
                    ProcLink) ->

    Prefix1 = case Prefix of
        "" -> "/";
        _ -> Prefix
    end,
    CouchdbParams1 = CouchdbParams#couchdb_params{prefix=Prefix1},
    InitialState = #server_state{couchdb = CouchdbParams1,
                                 prefix  = Prefix1,
                                 name=Name},
    {ok, Pid} = start_internal(InitialState, ProcLink),
    {ok, _} = couchbeam_manager:register_connection(Name, Pid),
    Pid.
    
start_internal(InitialState, _Link = true) ->
    gen_server:start_link(couchbeam_server, InitialState, []);
start_internal(InitialState, _Link = false) ->
    gen_server:start(couchbeam_server, InitialState, []).
    
info(ConnectionId) ->
    gen_server:call(ConnectionId, info).
    
%%---------------------------------------------------------------------------
%% DB operations
%%---------------------------------------------------------------------------

%% @spec all_dbs(ConnectionId::pid()) -> list()
%% @doc fetch list of all dbs
all_dbs(ConnectionId) ->
    Pid = maybe_managed(ConnectionId),
    gen_server:call(Pid, all_dbs, infinity).

%% @spec is_db(ConnectionId::pid(), DbName::string()) -> true|false
%% @doc If database exist in the node returns true    
is_db(ConnectionId, DbName) ->
    AllDbs = all_dbs(ConnectionId),
    lists:member(?l2b(DbName), AllDbs).

%% @spec create_db(ConnectionId::pid(), DbName::string()) -> ok
%% @doc create a database with DbName
create_db(ConnectionId, DbName) ->
    Pid = maybe_managed(ConnectionId),
    gen_server:call(Pid, {create_db, DbName}, infinity).
    
open_db(ConnectionId, DbName) ->
    Pid = maybe_managed(ConnectionId),
    gen_server:call(Pid, {open_db, DbName}, infinity).

%% @spec delete_db(ConnectionId::pid(), DbName::string()) -> ok
%% @doc delete a database with dbname    
delete_db(ConnectionId, DbName) ->
    Pid = maybe_managed(ConnectionId),
    gen_server:call(Pid, {delete_db, DbName}, infinity).
        
open_or_create_db(ConnectionId, DbName) ->
    Pid = case open_db(ConnectionId, DbName) of
        not_found ->
            create_db(ConnectionId, DbName);
        Pid1 -> Pid1
    end,
    Pid.
    
close_db(ConnectionPid, DbPid) ->
    Pid = maybe_managed(ConnectionPid),
    gen_server:call(Pid, {close_db, DbPid}, infinity).
    
  
%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------
%% @private
  
init(#server_state{couchdb=C, prefix=P} = InitialState) ->
    DbsByNameTid = ets:new(couch_dbs_by_name, [set, private]),
    DbsByPidTid = ets:new(couch_dbs_by_pid, [set, private]),
    {ok, UuidsPid} = gen_server:start_link(couchbeam_uuids, {C, P}, []),
    
    State = InitialState#server_state{dbs_by_name   = DbsByNameTid,
                                      dbs_by_pid    = DbsByPidTid,
                                      uuids_pid     = UuidsPid},
    process_flag(trap_exit, true),
    {ok, State}.
    
handle_call(info, _From, #server_state{prefix=Base, couchdb=C}=State) ->
    {ok, {Infos}} = couchbeam_resource:get(C, Base, [], [], []),
    {reply, Infos, State};
    
handle_call(all_dbs, _From, #server_state{prefix=Base, couchdb=C}=State) ->
    {ok, AllDbs} = couchbeam_resource:get(C, Base ++ "_all_dbs", [], [], []),
    {reply, AllDbs, State};

handle_call({open_db, DbName}, _From, #server_state{prefix=Base, 
                                            couchdb=C,
                                            dbs_by_name=DbsNameTid,
                                            dbs_by_pid=DbsPidTid}=State) ->
    Pid = case ets:lookup(DbsNameTid, DbName) of
        [] ->
            case couchbeam_resource:get(C, Base ++ DbName, [], [], []) of
                {ok, _} ->
                    {ok, DbPid} = gen_server:start_link(couchbeam_db, {DbName, State}, []),
                    true = ets:insert(DbsNameTid, {DbName, DbPid}),
                    true = ets:insert(DbsPidTid, {DbPid, DbName}),
                    DbPid;
                {error, Reason} -> Reason
            end;
        [{_, DbPid1}] -> DbPid1
    end,
    {reply, Pid, State};
    
handle_call({create_db, DbName}, _From, #server_state{prefix=Base,
                                                couchdb=C,
                                                dbs_by_name=DbsNameTid,
                                                dbs_by_pid=DbsPidTid}=State) ->
    Pid = case ets:lookup(DbsNameTid, DbName) of
        [] ->
            case couchbeam_resource:put(C, Base ++ DbName, [], [], [], []) of
                ok ->
                    {ok, DbPid} = gen_server:start_link(couchbeam_db, {DbName, State}, []),
                    true = ets:insert(DbsNameTid, {DbName, DbPid}),
                    true = ets:insert(DbsPidTid, {DbPid, DbName}),
                    DbPid;
                {error, Reason} -> Reason
            end;
        [{_, DbPid1}] -> 
            case couchbeam_resource:put(C, Base ++ DbName, [], [], [], []) of
                ok -> DbPid1;
                {error, Reason} -> Reason
            end 
    end,
    {reply, Pid, State};
    
handle_call({delete_db, DbName}, _From, #server_state{prefix=Base, 
                                                couchdb=C,
                                                dbs_by_name=DbsNameTid,
                                                dbs_by_pid=DbsPidTid}=State) ->
    Resp = case ets:lookup(DbsNameTid, DbName) of
        [] ->
            couchbeam_resource:delete(C, Base ++ DbName, [], [], []);
        [{_, Pid}] ->
            exit(Pid, kill),
            receive {'EXIT', Pid, _Reason} -> ok end,
            true = ets:delete(DbsNameTid, DbName),
            true = ets:delete(DbsPidTid, Pid),
            couchbeam_resource:delete(C, Base ++ DbName, [], [], [])
    end,
    {reply, Resp, State};
    
handle_call({close_db, DbPid}, _From, #server_state{dbs_by_name=DbsNameTid,
                                                    dbs_by_pid=DbsPidTid}=State) ->
    Resp = case ets:lookup(DbsPidTid, DbPid) of
        [] -> ok;
        [{_, DbName}] ->
            exit(DbPid, kill),
            receive {'EXIT', _Pid, _Reason} -> ok end,
            true = ets:delete(DbsPidTid, DbPid),
            true = ets:delete(DbsNameTid, DbName),
            ok
    end,
    {reply, Resp, State}.
    
    
handle_cast(_Msg, State) ->
    {no_reply, State}.
    
handle_info({'EXIT', Pid, _Reason}, #server_state{dbs_by_name=DbsNameTid,
                                                dbs_by_pid=DbsPidTid}=State) ->
    [{Pid, DbName}] = ets:lookup(DbsPidTid, Pid),
    [{DbName, Pid}] = ets:lookup(DbsNameTid, DbName),
    true = ets:delete(DbsPidTid, Pid),
    true = ets:delete(DbsNameTid, DbName),
    {noreply, State};
    
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 
    
      
%% @private
maybe_managed(ConnectionId) when is_pid(ConnectionId) ->
    ConnectionId;
maybe_managed(ConnectionId) ->
    couchbeam_manager:get_connection(ConnectionId).    

