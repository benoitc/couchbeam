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

-module(couchbeam_sup).
-author('Benoît Chesneau <benoitc@e-engura.org').
-behaviour(supervisor).

-export([start_link/0, stop/0, init/1, start_nodes/0]).

-include("couchbeam.hrl").

start_link() ->
    case whereis(couchbeam_sup) of
    undefined -> 
        start_couchbeam();
    _Else -> 
        {error, already_started}
    end.

start_couchbeam() ->
    ChildSpecs = 
    {{one_for_all, 10, 3600},
    [{couchbeam,
        {couchbeam, sup_start_link, []},
        permanent,
        brutal_kill,
        supervisor,
        [couchbeam]},
    {couchbeam_nodes,
        {couchbeam_sup, start_nodes, []},
        permanent,
        infinity,
        supervisor,
        [couchbeam_sup]}]},
    application:start(crypto),
    supervisor:start_link({local, couchbeam_sup}, couchbeam_sup, ChildSpecs).
    
start_nodes() ->
    Pid = supervisor:start_link({local, couchbeam_nodes}, couchbeam_sup,
        {{one_for_one, 10, 3600}, []}),
    %% add default node
    couchbeam:open_connection({default, {"127.0.0.1", 5984}}),
    Pid.
    
stop() ->
    catch exit(whereis(couchbeam_sup), normal).

init(ChildSpecs) ->
    {ok, ChildSpecs}.