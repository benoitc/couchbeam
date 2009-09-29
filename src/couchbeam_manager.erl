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
%%
%% @doc Manager for couchbeam
%% This gen_server is responsible of maintaining registration of server connections.

-module(couchbeam_manager).
-author('Benoît Chesneau <benoitc@e-engura.org').

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
    {ok, #couchbeam_manager{}}.
    

%% @hidden
handle_call({register, Name, ConnectionPid}, _, #couchbeam_manager{connections=Connections} = State) ->
    {R, NewState} = case dict:is_key(Name, Connections) of
        true -> {already_registered, State};
        false -> 
            Connections1 = dict:append(Name, ConnectionPid, Connections),
            {ok, #couchbeam_manager{connections=Connections1}}
    end,
    {reply, R, NewState};

handle_call({unregister, Name}, _, #couchbeam_manager{connections=Connections} = State) ->
    {R, NewState} = case dict:is_key(Name, Connections) of
        false -> {notfound, State};
        true -> 
            Connections1 = dict:erase(Name, Connections),
            {ok, #couchbeam_manager{connections=Connections1}}
    end,
    {reply, R, NewState};
    
handle_call({connection, Name}, _, #couchbeam_manager{connections=Connections} = State) ->
    [ConnPid] = case dict:is_key(Name, Connections) of
        false -> [not_found];
        true ->
            dict:fetch(Name, Connections)
    end,
    {reply, ConnPid, State};
    
handle_call(connection_count, _, State) ->
     {reply, dict:size(State#couchbeam_manager.connections), State}.
            
handle_cast(_Msg, State) ->
    {no_reply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 


