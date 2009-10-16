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
%% @doc Manager for couchbeam
%% This gen_server is responsible of maintaining registration of server connections.

-module(couchbeam_manager).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-behaviour(gen_server).

-include("couchbeam.hrl").

-export([start_link/0]).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2]).
  
-export([register_connection/2, unregister_connection/1, get_connection/1, 
         connection_count/0, register_db/3, unregister_db/1, get_db/1]).
  
%%---------------------------------------------------------------------------
%% manager operations
%%---------------------------------------------------------------------------
  
register_connection(ConnectionPid, State) ->
     gen_server:call(?MODULE, {register, ConnectionPid, State}).
     
unregister_connection(Name) ->
     gen_server:call(?MODULE, {unregister, Name}).
     
get_connection(Name) ->
     gen_server:call(?MODULE, {connection, Name}).   
     
connection_count() ->
    gen_server:call(?MODULE, connection_count).
    
register_db(ServerName, Name, DbPid) ->
    gen_server:call(?MODULE, {register_db, ServerName, Name, DbPid}).
 
unregister_db(Name) ->
    gen_server:call(?MODULE, {unregister_db, Name}).
    
get_db(Name) ->
    gen_server:call(?MODULE, {db, Name}).
    
%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------
%% @private

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).

init(_) ->
    process_flag(priority, high),
    {ok, #couchbeam_manager{}}.
    

%% @hidden
handle_call({register, ConnectionPid, #couchdb_params{name=Name}=CouchDBState}, _, 
                #couchbeam_manager{conns=Conns,refs=Refs}=State) ->
    {R, State1}= case dict:find(Name, Conns) of
        {ok, {_, MainPid}} ->
            {{already_registered, MainPid}, State};
        error ->
                Ref = erlang:monitor(process, ConnectionPid),
                Refs1 = dict:store(Ref, {connection, CouchDBState}, Refs),
                Conns1 = dict:store(Name, {ConnectionPid, Ref}, Conns),
                NewState = State#couchbeam_manager{conns=Conns1, refs=Refs1},
                {{ok, ConnectionPid}, NewState}
    end,
    {reply, R, State1};

handle_call({unregister, Name}, _, #couchbeam_manager{conns=Conns,refs=Refs}=State) ->
   {R, State1} = case  dict:find(Name, Conns) of
        {ok, {_, Ref}} ->
            Conns1 = dict:erase(Name, Conns),
            Refs1 = dict:erase(Ref, Refs),
            erlang:demonitor(Ref),
            {ok, State#couchbeam_manager{conns=Conns1, refs=Refs1}};
        error -> {not_found, State}
    end,
    {reply, R, State1};
    
handle_call({connection, Name}, _, #couchbeam_manager{conns=Conns}=State) ->
    R = case dict:find(Name, Conns) of
        {ok, {Pid, _}} -> Pid;
        error -> not_found
    end,
    {reply, R, State};
    
handle_call(connection_count, _, #couchbeam_manager{conns=Conns}=State) ->
    Size = dict:size(Conns),
    {reply, Size, State};
     
     
handle_call({register_db, ServerName, {Alias, _Name}=DbName, DbPid}, _, 
                                #couchbeam_manager{dbs=Dbs,refs=Refs}=State) ->
    {R, State1} = case dict:find(Alias, Dbs) of
        {ok, _} -> {already_registered, State};
        error ->
            Ref = erlang:monitor(process, DbPid),
            Refs1 = dict:store(Ref, {db, ServerName, DbName}, Refs),
            Dbs1 = dict:store(Alias, {DbPid, DbName, ServerName, Ref}, Dbs),
            {ok, State#couchbeam_manager{dbs=Dbs1,refs=Refs1}}
    end,
    {reply, R, State1};

handle_call({unregister_db, Name}, _, #couchbeam_manager{dbs=Dbs,refs=Refs}=State) ->
   {R, State1} = case dict:find(Name, Dbs) of
        {ok, {_,_,_,Ref}}-> 
            Dbs1 = dict:erase(Name, Dbs),
            Refs1 = dict:erase(Ref, Refs),
            erlang:demonitor(Ref),
            {ok, State#couchbeam_manager{dbs=Dbs1,refs=Refs1}};
        error -> {not_found, State}
    end,
    {reply, R, State1};    
            
handle_call({db, Name}, _, #couchbeam_manager{dbs=Dbs}=State) ->
    R = case dict:find(Name, Dbs) of
        {ok, {Pid, _, _, _}} -> Pid;
        error -> not_found
    end,
    {reply, R, State}.
            
handle_cast(_Msg, State) ->
    {no_reply, State}.
    
handle_info({'DOWN', _, _, _, normal}, State) ->
    {noreply, State};
handle_info({'DOWN', _, _, _, killed}, State) ->
    {noreply, State};
handle_info({'DOWN', Ref, _, _, _Reason}, #couchbeam_manager{conns=Conns,
                                                             dbs=Dbs,
                                                             refs=Refs}=State) ->
    State1 = case dict:find(Ref, Refs) of
        {ok, {connection, Params}} ->
            Refs1 = dict:erase(Ref, Refs),
            #couchdb_params{name=Name, prefix=Prefix} = Params,
            InitialState = #server_state{couchdb = Params,
                                         prefix  = Prefix,
                                         name=Name},
            {ok, Pid} = gen_server:start_link(couchbeam_server, InitialState, []),
            Ref1 = erlang:monitor(process, Pid),
            Refs2 = dict:store(Ref, {connection, Params}, Refs1),
            Conns1 = dict:store(Name, {Pid, Ref1}, Conns),
            State#couchbeam_manager{conns=Conns1,dbs=Dbs,refs=Refs2};
        {ok, {db, {ServerName, DbName}}} ->
            Refs1 = dict:erase(Ref, Refs),
            {Alias, _} = DbName,
            case dict:find(ServerName, Conns) of
                {ok, {Pid, _}} ->
                    DbPid = couchbeam_server:open_db(Pid, DbName, false),
                    Ref1 = erlang:monitor(process, DbPid),
                    Refs2 = dict:store(Ref1, {db, ServerName, DbName}, Refs1),
                    Dbs1 = dict:store(Alias, {DbPid, DbName, ServerName, Ref}, Dbs),
                    State#couchbeam_manager{conns=Conns,dbs=Dbs1,refs=Refs2};
                error -> State
            end;
        error ->
            State
    end,
    {noreply, State1};    
    
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 
