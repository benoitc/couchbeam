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

-module(couchbeam).
-author('Benoît Chesneau <benoitc@e-engura.org').

-behaviour(application).

-export([start/0, version/0]).
-export([json_encode/1,json_decode/1]).
-export([start/2, stop/1]).

%% --------------------------------------------------------------------
%% Generic utilities.
%% --------------------------------------------------------------------

    
%% @spec json_encode(V::json_term()) -> iolist()
%% @doc Encode to json
json_encode(V) ->
    couchbeam_mochijson2:encode(V).
    
%% @spec json_decode(V::iolist()) -> json_term()
%% @doc decode from json string
json_decode(V) ->
    couchbeam_mochijson2:decode(V).    
    
%% @spec () -> ok
%% @doc Start applications which exmpp depends on then start exmpp.
start() ->
    couchbeam_deps:ensure(),
    application:start(ssl),
    application:start(crypto),
    application:start(lhttpc),
    application:start(couchbeam).  
 
%% @spec () -> Version
%%     Version = string()
%% @doc Return the version of the application.
version() ->
    {ok, Version} = application:get_key(couchbeam, vsn),
    Version.   

%% --------------------------------------------------------------------
%% application(3erl) callbacks.
%% --------------------------------------------------------------------

%% @hidden

start(_Start_Type, _Start_Args) ->
    
    couchbeam_sup:start_link().

%% @hidden

stop(_State) ->
    ok.
    
