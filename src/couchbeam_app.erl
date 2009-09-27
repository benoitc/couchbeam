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

-module(couchbeam_app).
-author('Benoît Chesneau <benoitc@e-engura.org').


-behaviour(application).
-export([start/0,start/2,stop/0,stop/1]).

% callback to start application in shell
start() ->
    couchbeam_sup:start_link().
    
% callback to stop application in shell
stop() ->
    couchbeam_sup:stop().

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for webhub.
start(_Type, _StartArgs) ->
    start().
     
%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for webhub.
stop(_Reason) ->
    stop().