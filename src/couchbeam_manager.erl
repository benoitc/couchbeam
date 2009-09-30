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
         connection_count/0]).
  
%%---------------------------------------------------------------------------
%% manager operations
%%---------------------------------------------------------------------------
  
register_connection(Name, ConnectionPid) ->
     gen_server:call(?MODULE, {register, Name, ConnectionPid}).
     
unregister_connection(Name) ->
     gen_server:call(?MODULE, {unregister, Name}).
     
get_connection(Name) ->
     gen_server:call(?MODULE, {connection, Name}).   
     
connection_count() ->
    gen_server:call(?MODULE, connection_count).
    
%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------
%% @private

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).
    
    
init(_) ->
    process_flag(priority, high),
    Connections = ets:new(couchdbeam_conns_by_name, [set, private, named_table]),
    {ok, #couchbeam_manager{connections=Connections}}.
    

%% @hidden
handle_call({register, Name, ConnectionPid}, _, State) ->
    R = case ets:lookup(couchdbeam_conns_by_name, Name) of
        [] -> 
            true = ets:insert(couchdbeam_conns_by_name, {Name, ConnectionPid}),
            {ok, ConnectionPid};
        [{_, MainPid}] ->
            {already_registered, MainPid}
    end,
    {reply, R, State};

handle_call({unregister, Name}, _, State) ->
   R = case ets:lookup(couchdbeam_conns_by_name, Name) of
        [] -> {notfound, State};
        [{_,_}] -> 
            ets:delete(couchdbeam_conns_by_name, Name),
            ok
    end,
    {reply, R, State};
    
handle_call({connection, Name}, _, State) ->
    R = case ets:lookup(couchdbeam_conns_by_name, Name) of
        [] -> not_found;
        [{_, Pid}] -> Pid   
    end,
    {reply, R, State};
    
handle_call(connection_count, _, State) ->
    Infos = ets:info(couchdbeam_conns_by_name),
    Size = proplists:get_value(size, Infos),
     {reply, Size, State}.
            
handle_cast(_Msg, State) ->
    {no_reply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 
