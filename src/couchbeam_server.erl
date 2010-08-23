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
-export([info/1, close/1, create_db/2, open_db/2, open_db/3, open_or_create_db/2, 
         delete_db/2, close_db/2, all_dbs/1, is_db/2]).
         
 
start_connection() -> start_connection(#couchdb_params{}).

start_connection(Params) -> start_connection_internal(Params, false).

start_connection_link() -> start_connection_link(#couchdb_params{}).

start_connection_link(Params) -> start_connection_internal(Params, true).        

start_connection_internal(CouchdbParams,ProcLink) ->
    #couchdb_params{
        name=Name,
        prefix=Prefix,
        max_dbs_open=MaxDbsOpen
    } = CouchdbParams,

    Prefix1 = case Prefix of
        "" -> "/";
        _ -> Prefix
    end,
    CouchdbParams1 = CouchdbParams#couchdb_params{prefix=Prefix1},
    InitialState = #server{couchdb = CouchdbParams1,
                           name = Name,
                           prefix  = Prefix1,
                           max_dbs_open=MaxDbsOpen},
    start_internal(InitialState, ProcLink).
    
start_internal(#server{name=Name}=InitialState, _Link = true) ->
    Conn = {
        Name,
        {gen_server, start_link,
            [?MODULE, [InitialState], []]},
        permanent,
        1000,
        worker,
        [?MODULE]
    },

    case supervisor:start_child(couchbeam_sup, Conn) of
    {ok, Pid} ->
        Pid;
    {error, already_present} ->
        case supervisor:restart_child(couchbeam_sup, Conn) of
        {ok, Pid} ->
            Pid;
        {error, running} ->
            {error, {already_started, Pid}} =
                supervisor:start_child(couchbeam_sup, Conn),
            Pid
        end;
    {error, {already_started, Pid}} ->
        Pid
    end;
start_internal(InitialState, _Link = false) ->
    {ok, Pid} = gen_server:start(couchbeam_server, InitialState, []),
    Pid.


close(ConnectionPid) ->
    try
        gen_server:call(ConnectionPid, close)
    catch
        exit:_ -> ok
    end.
    
info(ConnectionPid) ->
    gen_server:call(ConnectionPid, info).
    
%%---------------------------------------------------------------------------
%% DB operations
%%---------------------------------------------------------------------------

%% @spec all_dbs(ConnectionId::pid()) -> list()
%% @doc fetch list of all dbs
all_dbs (ConnectionPid) ->
    gen_server:call(ConnectionPid, all_dbs, infinity).

%% @spec is_db(ConnectionId::pid(), DbName::string()) -> true|false
%% @doc If database exist in the node returns true    
is_db(ConnectionPid, DbName) ->
    AllDbs = all_dbs(ConnectionPid),
    lists:member(list_to_binary(DbName), AllDbs).

%% @spec create_db(ConnectionId::pid(), DbName::string()) -> ok
%% @doc create a database with DbName
create_db(ConnectionPid, DbName) ->
    create_db(ConnectionPid, DbName, []).
    
create_db(ConnectionPid, DbName, Options) ->
    gen_server:call(ConnectionPid, {create_db, db_name(DbName), Options}, infinity).
    
open_db(ConnectionPid, DbName) ->
    open_db(ConnectionPid, DbName, []).
    
open_db(ConnectionPid, DbName, Options) ->
    gen_server:call(ConnectionPid, {open_db, db_name(DbName), Options}, infinity).

%% @spec delete_db(ConnectionId::pid(), DbName::string()) -> ok
%% @doc delete a database with dbname    
delete_db(ConnectionPid, DbName) ->
    gen_server:call(ConnectionPid, {delete_db, 
            couchbeam_util:url_encode(DbName)}, infinity).
        
open_or_create_db(ConnectionPid, DbName) ->
    open_or_create_db(ConnectionPid, DbName, []).
    
open_or_create_db(ConnectionPid, DbName, Options) ->
    case open_db(ConnectionPid, DbName, Options) of
        not_found ->
            create_db(ConnectionPid, DbName, Options);
        Db -> Db
    end.

close_db(ConnectionPid, DbName) ->
    gen_server:call(ConnectionPid, {close_db, db_name(DbName)}, infinity).
    
check_dbname(#server{dbname_regexp=RegExp}, DbName) ->
    case re:run(DbName, RegExp, [{capture, none}]) of
    nomatch ->
        {error, illegal_database_name};
    match ->
        ok
    end.
  
%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------
%% @private
  
init([#server{couchdb=C, prefix=P} = InitialState]) ->
    ets:new(couch_dbs_by_name, [set, private, named_table]),
    ets:new(couch_dbs_by_pid, [set, private, named_table]),
    ets:new(couch_dbs_by_lru, [ordered_set, private, named_table]),

    {ok, UuidsPid} = gen_server:start_link(couchbeam_uuids, {C, P}, []),
    {ok, RegExp} = re:compile("^[a-z][a-z0-9\\_\\$()\\+\\-\\/]*$"),

    State = InitialState#server{
        uuids_pid= UuidsPid,
        dbname_regexp = RegExp
    },
    process_flag(trap_exit, true),
    {ok, State}.
    
handle_call(info, _From, #server{prefix=Base, couchdb=C}=State) ->
    {ok, {Infos}} = couchbeam_resource:get(C, Base, [], [], []),
    {reply, Infos, State};
    
handle_call(close, _, State) ->
    {stop, normal, State};
    
handle_call(all_dbs, _From, #server{prefix=Base, couchdb=C}=State) ->
    {ok, AllDbs} = couchbeam_resource:get(C, Base ++ "_all_dbs", [], [], []),
    {reply, AllDbs, State};

handle_call({open_db, DbName, Options}, _From, Server) ->
    LruTime = now(),
    case ets:lookup(couch_dbs_by_name, DbName) of
        [] ->
            do_open_db(DbName, Server, Options);
        [{_, {MainPid, PrevLruTime, _Ref}}] ->
            Ref = erlang:monitor(process, MainPid),
            true = ets:insert(couch_dbs_by_name, {DbName, {MainPid,
                        LruTime, Ref}}),
            true = ets:delete(couch_dbs_by_lru, PrevLruTime),
            true = ets:insert(couch_dbs_by_lru, {LruTime, DbName}),
            {reply, MainPid, Server}
    end;
    
handle_call({create_db, DbName, Options}, _From, Server) ->
    #server{prefix=Base, couchdb=C} = Server, 
    case check_dbname(Server, DbName) of
        ok ->
            case ets:lookup(couch_dbs_by_name, DbName) of
            [] ->
                case couchbeam_resource:put(C, Base ++ DbName, [], [], 
                        [], []) of
                    ok ->
                        do_open_db(DbName, Server, Options);
                    {error, Reason} ->
                        {reply, Reason, Server}
                end;
            [_AlreadyRunningDb] ->
                {reply, db_exists, Server}
            end;
        Error ->
            {reply, Error, Server}
    end;

handle_call({delete_db, DbName}, _From, Server) ->
    #server{prefix=Base,couchdb=C} = Server,       
    case ets:lookup(couch_dbs_by_name, DbName) of
    [] ->
        couchbeam_resource:delete(C, Base ++ DbName, [], [], []),
        {reply, ok, Server};
    [{_, {Pid, LruTime, Ref}}] ->
        erlang:demonitor(Ref, [flush]),
        couchbeam_util:shutdown_sync(Pid),
        true = ets:delete(couch_dbs_by_name, DbName),
        true = ets:delete(couch_dbs_by_pid, Pid),
        true = ets:delete(couch_dbs_by_lru, LruTime),
        couchbeam_resource:delete(C, Base ++ DbName, [], [], []),
        DbsOpen = Server#server.dbs_open - 1,
        {reply, ok, Server#server{dbs_open=DbsOpen}} 
    end;
   
handle_call({close_db, DbName}, _From, Server) ->
    case ets:lookup(couch_dbs_by_name, DbName) of
        [] -> ok;
        [{_, {_Pid, _LruTime, Ref}}] -> erlang:demonitor(Ref, [flush])
    end,
    {reply, ok, Server};

handle_call({close_db_pid, DbPid}, _From, Server) ->
    case ets:lookup(couch_dbs_by_pid, DbPid) of 
    [] -> ok;
    [DbName] ->
        case ets:lookup(couch_dbs_by_name, DbName) of
        [] -> ok;
        [{_, {_Pid, _LruTime, Ref}}] ->
            erlang:demonitor(Ref, [flush])
        end
    end,
    {reply, ok, Server}.

handle_cast(Msg, _Server) ->
    exit({unknown_cast_message, Msg}).

handle_info({'DOWN', _MonRef, _, Pid, _}, Server) ->
    case ets:lookup(couch_dbs_by_pid, Pid) of 
    [] -> ok;
    [DbName] ->
        ets:delete(couch_dbs_by_pid, Pid),
        case ets:lookup(couch_dbs_by_name, DbName) of
        [] -> ok;
        [{_, {_Pid, LruTime, _Ref}}] ->
            ets:delete(couch_dbs_by_name, DbName),
            ets:delete(couch_dbs_by_lru, LruTime)
        end
    end,
    DbsOpen = Server#server.dbs_open - 1,
    {noreply, Server#server{dbs_open=DbsOpen}};

handle_info(Error, _Server) ->
    io:format("Unexpected message, restarting couchbeam_server: ~p", [Error]),
    exit(kill).

terminate(_Reason, _State) ->
    % close all dbs related to this server
    [couchbeam_util:shutdown_sync(Pid) || {_, {Pid, _LruTime}} <-
            ets:tab2list(couch_dbs_by_name)],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 
    
      
%% @private
maybe_close_lru(#server{dbs_open=DbsOpen,
        max_dbs_open=MaxDbsOpen}=Server) when DbsOpen < MaxDbsOpen ->
    {ok, Server};
maybe_close_lru(#server{dbs_open=DbsOpen}=Server) ->
    % must free up the lru db.
    case try_close_lru(now()) of
    ok ->
        {ok, Server#server{dbs_open=DbsOpen - 1}};
    Error -> Error
    end.

try_close_lru(StartTime) ->
    LruTime = get_lru(),
    if LruTime > StartTime ->
        % this means we've looped through all our opened dbs and found them
        % all in use.
        {error, all_dbs_active};
    true ->
        [{_, DbName}] = ets:lookup(couch_dbs_by_lru, LruTime),
        [{_, {MainPid, LruTime}}] = ets:lookup(couch_dbs_by_name, DbName),
        case couch_db:is_idle(MainPid) of
        true ->
            ok = shutdown_idle_db(DbName, MainPid, LruTime);
        false ->
            % this still has referrers. Go ahead and give it a current lru time
            % and try the next one in the table.
            NewLruTime = now(),
            true = ets:insert(couch_dbs_by_name, {DbName, {MainPid, NewLruTime}}),
            true = ets:insert(couch_dbs_by_pid, {MainPid, DbName}),
            true = ets:delete(couch_dbs_by_lru, LruTime),
            true = ets:insert(couch_dbs_by_lru, {NewLruTime, DbName}),
            try_close_lru(StartTime)
        end
    end.

get_lru() ->
    get_lru(ets:first(couch_dbs_by_lru)).

get_lru(LruTime) ->
    [{LruTime, DbName}] = ets:lookup(couch_dbs_by_lru, LruTime),

    [{_, {MainPid, _, _}}] = ets:lookup(couch_dbs_by_name, DbName),
    case couch_db:is_idle(MainPid) of
    true ->
        NextLru = ets:next(couch_dbs_by_lru, LruTime),
        ok = shutdown_idle_db(DbName, MainPid, LruTime),
        get_lru(NextLru);
    false ->
        get_lru(ets:next(couch_dbs_by_lru, LruTime))
    end.


shutdown_idle_db(DbName, MainPid, LruTime) ->
    couchbeam_util:shutdown_sync(MainPid),
    true = ets:delete(couch_dbs_by_lru, LruTime),
    true = ets:delete(couch_dbs_by_name, DbName),
    true = ets:delete(couch_dbs_by_pid, MainPid),
    ok.

do_open_db(DbName, Server, _Options) ->
    #server{prefix=Base, couchdb=C} = Server,
    case couchbeam_resource:get(C, Base, [], [], []) of
        {ok, _} -> 
            case maybe_close_lru(Server) of
            {ok, Server2} ->
                {ok, DbPid} = gen_server:start_link(couchbeam_db,
                    {DbName, Server2, self()}, []),
                LruTime = now(),
                Ref = erlang:monitor(process, DbPid),
                true = ets:insert(couch_dbs_by_name, {DbName, {DbPid,
                            LruTime, Ref}}),
                true = ets:insert(couch_dbs_by_pid, {DbPid, DbName}),
                true = ets:insert(couch_dbs_by_lru, {LruTime, DbName}),
                DbsOpen = Server2#server.dbs_open + 1,
                {reply, DbPid, Server2#server{dbs_open=DbsOpen}};
            CloseError ->
                {reply, CloseError, Server}
            end;
        {error, Error} ->
            {reply, Error, Server}
    end.

db_name(Name) when is_list(Name) ->
    Name;
db_name(Name) when is_binary(Name) ->
    db_name(binary_to_list(Name));
db_name(Name)  ->
    db_name(atom_to_list(Name)).
