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
%%% start_app has been borrowed to couchdb project under Apache2 license
%%%

%% @author Benoît Chesneau <benoitc@e-engura.org>
%% @copyright 2009 Benoît Chesneau.


-module(couchbeam).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-behaviour(application).

-include("couchbeam.hrl").

-export([start/0, version/0]).
-export([start/2, stop/1]).
-export([transact/1, transact/2, transact/3, transact/4]).

%% --------------------------------------------------------------------
%% transact functions
%% --------------------------------------------------------------------

%% @spec transact(Fun) -> {error, Reason} | Result
%% @doc This function s the functional object Fun with the ConnctionPid
%% as argument. In this case default params are used.
transact(F) ->
    case couchbeam_server:start_connection_link() of
        {error, Error} -> {error, Error};
        Conn -> transact(Conn, F)
    end.


%% @spec transact(Params, Fun) -> {error, Reason} | Result
%% @doc This function s the functional object Fun with the ConnctionPid
%% as argumentt. Params is a #couchdb_params{} record
transact(Params, F) when is_record(Params, couchdb_params) ->
    case couchbeam_server:start_connection_link(Params) of
        {error, Error} -> {error, Error};
        Conn -> F(Conn)
    end;

%% @spec transact(ConnectionPid, Fun) -> {error, Reason} | Result
%% @doc This function s the functional object Fun with the ConnctionPid
%% as argumentt.
transact(Conn, F) when is_pid(Conn) ->
    F(Conn);

%% @spec transact(iName, Fun) -> {error, Reason} | Result
%% @doc This function s the functional object Fun with the ConnctionPid
%% as argumentt.The name of the connection is passed and we look in
%% supervised process to find the connection Pid
transact(Name, F) ->
    Children = supervisor:which_children(couchbeam_sup),
    case proplists:lookup(Name, Children) of
        none -> {error, not_started};
        {_, Conn, _, _} -> F(Conn)
    end.

%% @spec transact(Connection, DbName, Fun) -> {error, Reason} | Result
%% @doc This function s the functional object Fun with the Database Pid
%% as argumentt.
transact(Connection, DbName, F) ->
    transact(Connection, fun(Conn) ->
        case couchbeam_server:open_db(Conn, DbName) of
            {error, Error} -> {error, Error};
            Db -> F(Db)
        end
    end).
    
%% @spec transact(Connection, DbName, Options, Fun) -> {error, Reason} | Result
%% @doc This function s the functional object Fun with the Database Pid
%% as argumentt.
transact(Connection, DbName, Options, F) ->
    transact(Connection, fun(Conn) ->
        case couchbeam_server:open_db(Conn, DbName, Options) of
            {error, Error} -> {error, Error};
            Db -> F(Db)
        end
    end).

%% --------------------------------------------------------------------
%% Generic utilities.
%% --------------------------------------------------------------------

    
    
%% @spec () -> ok
%% @doc Start applications which exmpp depends on then start exmpp.
start() ->
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
    couchbeam_deps:ensure(),
    case start_apps([crypto, public_key, ssl, lhttpc]) of
        ok->
            couchbeam_sup:start_link();
        {error, Reason} ->
            {error, Reason}
    end.
    
%% @hidden

stop(_State) ->
    ok.
    
start_apps([]) ->
    ok;
start_apps([App|Rest]) ->
    case application:start(App) of
    ok ->
       start_apps(Rest);
    {error, {already_started, App}} ->
       start_apps(Rest);
    {error, _Reason} when App =:= public_key ->
       % ignore on R12B5
       start_apps(Rest);
    {error, _Reason} ->
       {error, {app_would_not_start, App}}
    end.
