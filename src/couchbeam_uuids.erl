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

-module(couchbeam_uuids).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-behaviour(gen_server).

-include("couchbeam.hrl").

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2]).
-export([uuid/1, uuids/2, next_uuid/1]).
     

%%---------------------------------------------------------------------------
%% uuids operations
%%---------------------------------------------------------------------------

uuid(Pid) ->
    uuids(Pid, 1).
    
uuids(Pid, Count) ->
    gen_server:call(Pid, {uuids, Count}, infinity).
    
next_uuid(Pid) ->
    gen_server:call(Pid, next_uuid, infinity).


%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------
%% @private
     
init({CouchdbParams, Base}) ->
    UuidsTid = ets:new(couch_uuids, [ordered_set, private]),
    State = #uuids{couchdb = CouchdbParams,
                   base = Base,
                   tid = UuidsTid},
    {ok, State}.
    
    
handle_call({uuids, Count}, _From, #uuids{couchdb=C, base=Base}=State) ->
    Uuids = case couchbeam_resource:get(C, Base ++ "_uuids", [], 
                            [{"count", couchbeam_util:val(Count)}], []) of
        {ok, {[{<<"uuids">>, Uuids1}]}} -> Uuids1;
        {error, Reason} -> Reason
    end,
    {reply, Uuids, State};
    
handle_call(next_uuid, _From, #uuids{couchdb=C, base=Base, tid=UuidsTid} = State) ->
    Uuid = case ets:lookup(UuidsTid, uuids) of
        [] -> new_uuids(C, Base, UuidsTid);
        [{_, []}] -> new_uuids(C, Base, UuidsTid);
        [{_, [Id2|Uuids]}] ->
            true = ets:insert(UuidsTid, {uuids, Uuids}),
            ?b2l(Id2)
    end,
    {reply, Uuid, State}.
    
handle_cast(_Msg, State) ->
    {no_reply, State}.
    

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 
    


%% @private
new_uuids(CouchdbParams, Base, UuidsTid) ->
    Uuid = case couchbeam_resource:get(CouchdbParams, Base ++ "_uuids", [], 
                            [{"count", "1000"}], []) of
        {ok, {[{<<"uuids">>, [Id|Uuids1]}]}} -> 
            ets:insert(UuidsTid, {uuids, Uuids1}),
            ?b2l(Id);
        {error, Reason} -> Reason
    end,
    Uuid.
    
