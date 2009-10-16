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
         connection_count/0, register_db/2, unregister_db/1, get_db/1]).
  
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
    
register_db(Name, DbPid) ->
    gen_server:call(?MODULE, {register_db, Name, DbPid}).
 
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
    Connections = ets:new(couchbeam_conns_by_name, [set, private, named_table]),
    Dbs = ets:new(couchbeam_dbs_by_name, [set, private, named_table]),
    Refs = ets:new(couchbeam_refs, [set, private, named_table]),
    {ok, #couchbeam_manager{connections=Connections, dbs=Dbs, refs=Refs}}.
    

%% @hidden
handle_call({register, ConnectionPid, #couchdb_params{name=Name}=CouchDBState}, _, State) ->
    R = case ets:lookup(couchbeam_conns_by_name, Name) of
        [] -> 
            Ref = erlang:monitor(process, ConnectionPid),
            true = ets:insert(couchbeam_refs, {Ref, {connection, CouchDBState}}),
            true = ets:insert(couchbeam_conns_by_name, {Name, {ConnectionPid, CouchDBState}}),
            {ok, ConnectionPid};
        [{_, MainPid}] ->
            {already_registered, MainPid}
    end,
    {reply, R, State};

handle_call({unregister, Name}, _, State) ->
   R = case ets:lookup(couchbeam_conns_by_name, Name) of
        [] -> not_found;
        [{_,_}] -> 
            ets:delete(couchbeam_conns_by_name, Name),
            ok
    end,
    {reply, R, State};
    
handle_call({connection, Name}, _, State) ->
    R = case ets:lookup(couchbeam_conns_by_name, Name) of
        [] -> not_found;
        [{_, {Pid, _}}] -> Pid   
    end,
    {reply, R, State};
    
handle_call(connection_count, _, State) ->
    Infos = ets:info(couchbeam_conns_by_name),
    Size = proplists:get_value(size, Infos),
     {reply, Size, State};
     
     
handle_call({register_db, Name, DbPid}, _, State) ->
    R = case ets:lookup(couchbeam_dbs_by_name, Name) of
        [] ->
            true = ets:insert(couchbeam_dbs_by_name, {Name, DbPid}),
            ok;
        [{_, _}] -> already_registered
    end,
    {reply, R, State};

handle_call({unregister_db, Name}, _, State) ->
   R = case ets:lookup(couchbeam_dbs_by_name, Name) of
        [] -> not_found;
        [{_,_}] -> 
            ets:delete(couchbeam_dbs_by_name, Name),
            ok
    end,
    {reply, R, State};    
            
handle_call({db, Name}, _, State) ->
    R = case ets:lookup(couchbeam_dbs_by_name, Name) of
        [] -> not_found;
        [{_, Pid}] -> Pid
    end,
    {reply, R, State}.
            
handle_cast(_Msg, State) ->
    {no_reply, State}.
    
handle_info({'DOWN', _, _, _, normal}, State) ->
    {noreply, State};

handle_info({'DOWN', Ref, _, _, _}, State) ->
    case ets:lookup(couchbeam_refs, Ref) of
        [] -> ok;
        [{_, {connection, Params}}] ->
              ets:delete(couchbeam_refs, Ref),
              #couchdb_params{name=Name, prefix=Prefix} = Params,
              InitialState = #server_state{couchdb = Params,
                                           prefix  = Prefix,
                                           name=Name},
              {ok, Pid} = gen_server:start_link(couchbeam_server, InitialState, []),
              Ref1 = erlang:monitor(process, Pid),
              true = ets:insert(couchbeam_refs, {Ref1, {connection, Params}}),
              true = ets:insert(couchbeam_conns_by_name, {Name, {Pid, Params}})
    end,
              
    {noreply, State};    
    
handle_info(_Info, State) ->
    io:format("got ~p ~n", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 
