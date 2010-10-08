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

-behaviour(gen_server).

-include("couchbeam.hrl").

-record(state, {}).



% generic functions
-export([start_link/0, start/0, stop/0, version/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% utilities urls 
-export([server_url/1, uuids_url/1, db_url/1, doc_url/2, make_url/3]).
-export([request/3, request/4, request/5,
         request_stream/3, request_stream/4, request_stream/5,
         db_request/3, db_request/4, db_request/5]).

%% API urls
-export([server_connection/0, server_connection/2, server_connection/4,
        server_connection/5, server_info/1,
        get_uuid/1, get_uuids/2, 
        all_dbs/1, db_exists/2,
        create_db/2, create_db/3, create_db/4, 
        open_db/2, open_db/3,
        open_or_create_db/2, open_or_create_db/3, open_or_create_db/4,
        delete_db/1, delete_db/2, 
        db_info/1,
        save_doc/2, save_doc/3,
        open_doc/2, open_doc/3,
        delete_doc/2, delete_doc/3,
        save_docs/2, save_docs/3, delete_docs/2, delete_docs/3,
        all_docs/1, all_docs/2, view/2, view/3,
        ensure_full_commit/1, ensure_full_commit/2,
        compact/1, compact/2]).

%% --------------------------------------------------------------------
%% Generic utilities.
%% --------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
%% @doc Starts the couchbeam process linked to the calling process. Usually 
%% invoked by the supervisor couchbeam_sup
%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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


%% @doc Start the couchbeam process. Useful when testing using the shell. 
start() ->
    couchbeam_deps:ensure(),
    case start_apps([crypto, public_key, sasl, ssl, ibrowse]) of
        ok ->
            couchbeam_sup:start_link();
        Error ->
            Error
    end.

%% @doc Stop the couchbeam process. Useful when testing using the shell. 
stop() ->
    application:stop(couchbeam),
    application:stop(ibrowse),
    application:stop(crypto).

 
%% @spec () -> Version
%%     Version = string()
%% @doc Return the version of the application.
version() ->
    {ok, Version} = application:get_key(couchbeam, vsn),
    Version.   

%% --------------------------------------------------------------------
%% API functins.
%% --------------------------------------------------------------------

%% @doc Create a server for connectiong to a CouchDB node
%% @equiv server_connection("127.0.0.1", 5984, "", [], false)
server_connection() ->
    #server{host="127.0.0.1", port=5984, ssl=false, prefix="",
        options=[]}.

%% @doc Create a server for connectiong to a CouchDB node
%% @equiv server_connection(Host, Port, "", [])

server_connection(Host, Port) ->
    server_connection(Host, Port, "", []).

%% @doc Create a server for connectiong to a CouchDB node
%% @equiv server_connection(Host, Port, "", [], Ssl)
server_connection(Host, Port, Prefix, Options) when is_integer(Port), Port =:=443 ->
    server_connection(Host, Port, Prefix, Options, true);
server_connection(Host, Port, Prefix, Options) ->
    server_connection(Host, Port, Prefix, Options, false).


%% @doc Create a server for connectiong to a CouchDB node
%% 
%%      Connections are made to:
%%      ```http://Host:PortPrefix'''
%%
%%      If ssl is set https is used.
%%
%% @spec server_connection(string(), integer(), string(), options(), boolean()) -> server() 
server_connection(Host, Port, Prefix, Options, Ssl) when is_binary(Port) ->
    server_connection(Host, binary_to_list(Port), Prefix, Options, Ssl);
server_connection(Host, Port, Prefix, Options, Ssl) when is_list(Port) ->
    server_connection(Host, list_to_integer(Port), Prefix, Options, Ssl); 
server_connection(Host, Port, Prefix, Options, Ssl) ->
    #server{host=Host, port=Port, ssl=Ssl, prefix=Prefix,
        options=Options}.

%% @doc Get Information from the server
%% @spec server_info(server()) -> iolist()
server_info(Server) ->
    Url = binary_to_list(iolist_to_binary(server_url(Server))),
    case request(get, Url, ["200"]) of
        {ok, _Status, _Headers, Body} ->
            Version = couchbeam_util:json_decode(Body),
            {ok, Version};
        Error -> Error
    end.

%% @doc Get one uuid from the server
%% @spec get_uuid(server()) -> lists()
get_uuid(Server) ->
    get_uuids(Server, 1).

%% @doc Get a list of uuids from the server
%% @spec get_uuids(server(), integer()) -> lists()
get_uuids(Server, Count) ->
    gen_server:call(couchbeam, {get_uuids, Server, Count}, infinity).
 
%% @doc get list of databases on a CouchDB node 
%% @spec all_dbs(server()) -> iolist()
all_dbs(Server) ->
    Url = make_url(Server, "_all_dbs", []),
    case request(get, Url, ["200"]) of
        {ok, _, _, Body} ->
            AllDbs = couchbeam_util:json_decode(Body),
            {ok, AllDbs};
        Error ->
            Error
    end.

%% @doc test if db with dbname exists on the CouchDB node
%% @spec db_exists(server(), string()) -> boolean()
db_exists(Server, DbName) ->
    Url = make_url(Server, DbName, []),
    case request(head, Url, ["200"]) of
        {ok, _, _, _} -> true;
        _Error -> false
    end.

%% @doc Create a database and a client for connectiong to it.
%% @equiv create_db(Server, DbName, [], [])
create_db(Server, DbName) ->
    create_db(Server, DbName, [], []).

%% @doc Create a database and a client for connectiong to it.
%% @equiv create_db(Server, DbName, Options, [])
create_db(Server, DbName, Options) ->
    create_db(Server, DbName, Options, []).

%% @doc Create a database and a client for connectiong to it.
%% 
%%      Connections are made to:
%%      ```http://Host:PortPrefix/DbName'''
%%
%%      If ssl is set https is used.
%%
%% @spec create_db(sserver(), string(), options(), list()) -> db() 
create_db(Server, DbName, Options, Params) ->
    Url = make_url(Server, DbName, Params),
    case request(put, Url, ["201"]) of
        {ok, _Status, _Headers, _Body} ->
            {ok, #db{server=Server, name=DbName, options=Options}};
        {error, {ok, "412", _, _}} ->
            {error, db_exists};
       Error ->
          Error
    end. 

%% @doc Create a client for connection to a database
%% @equiv open_db(Server, DbName, [])
open_db(Server, DbName) ->
    open_db(Server, DbName, []).

%% @doc Create a client for connection to a database
%% @spec open_db(server(), string(), list()) -> db() 
open_db(Server, DbName, Options) ->
    {ok, #db{server=Server, name=DbName, options=Options}}.
    

%% @doc Create a client for connecting to a database and create the
%%      database if needed.
%% @equiv open_or_create_db(Server, DbName, [], [])
open_or_create_db(Server, DbName) ->
    open_or_create_db(Server, DbName, [], []).

%% @doc Create a client for connecting to a database and create the
%%      database if needed.
%% @equiv open_or_create_db(Server, DbName, Options, [])
open_or_create_db(Server, DbName, Options) ->    
    open_or_create_db(Server, DbName, Options, []).

%% @doc Create a client for connecting to a database and create the
%%      database if needed.
%% @spec open_or_create_db(server(), string(), list(), list()) -> db()
open_or_create_db(Server, DbName, Options, Params) ->
    Url = make_url(Server, DbName, []),
    case request(get, Url, ["200"]) of
        {ok, _, _, _} ->
            open_db(Server, DbName, Options);
        {error, {ok, "404", _, _}} ->
            create_db(Server, DbName, Options, Params);
        Error ->
            Error
    end.

%% @doc delete database 
%% @equiv delete_db(Server, DbName)
delete_db(#db{server=Server, name=DbName}) ->
    delete_db(Server, DbName).

%% @doc delete database 
%% @spec delete_db(server(), DbName) -> iolist()
delete_db(Server, DbName) ->
    Url = make_url(Server, DbName, []),
    case request(delete, Url, ["200"]) of
        {ok, _, _, Body} ->
            {ok, couchbeam_util:json_decode(Body)};
        Error ->
            Error
    end.

%% @doc get database info
%% @spec db_info(db()) -> iolist()
db_info(#db{server=Server, name=DbName}) ->
    Url = make_url(Server, DbName, []),
    case request(get, Url, ["200"]) of
        {ok, _Status, _Headers, Body} ->
            Infos = couchbeam_util:json_decode(Body),
            {ok, Infos}; 
        {error, {ok, "404", _, _}} ->
            {error, db_not_found};
       Error ->
          Error
    end.

open_doc(Db, DocId) ->
    open_doc(Db, DocId, []).
open_doc(#db{server=Server}=Db, DocId, Params) ->
    DocId1 = couchbeam_util:encode_docid(DocId), 
    Url = make_url(Server, doc_url(Db, DocId1), Params),
    case db_request(get, Url, ["200", "201"]) of
        {ok, _, _, Body} ->
            {ok, couchbeam_util:json_decode(Body)};
        Error ->
            Error
    end.

save_doc(Db, Doc) ->
    save_doc(Db, Doc, []).
save_doc(#db{server=Server}=Db, {Props}=Doc, Options) ->
    DocId = case proplists:get_value(<<"_id">>, Props) of
        undefined ->
            [Id] = get_uuid(Server),
            Id;
        DocId1 ->
            couchbeam_util:encode_docid(DocId1)
    end,
    Url = make_url(Server, doc_url(Db, DocId), Options),
    Body = couchbeam_util:json_encode(Doc),
    Headers = [{"Content-Type", "application/json"}],
    case db_request(put, Url, ["201", "202"], Headers, Body) of
        {ok, _, _, RespBody} ->
            {JsonProp} = couchbeam_util:json_decode(RespBody),
            NewRev = proplists:get_value(<<"rev">>, JsonProp),
            NewDocId = proplists:get_value(<<"id">>, JsonProp),
            Doc1 = couchbeam_doc:set_value(<<"_rev">>, NewRev, 
                couchbeam_doc:set_value(<<"_id">>, NewDocId, Doc)),
            {ok, Doc1};
        Error -> 
            Error
    end.

delete_doc(Db, Doc) ->
    delete_doc(Db, Doc, []).

delete_doc(Db, Doc, Options) ->
    delete_docs(Db, [Doc], Options).

delete_docs(Db, Docs) ->
    delete_docs(Db, Docs, []).
delete_docs(Db, Docs, Options) ->
    Docs1 = lists:map(fun({DocProps})->
        {[{<<"_deleted">>, true}|DocProps]}
        end, Docs),
    save_docs(Db, Docs1, Options).

save_docs(Db, Docs) ->
    save_docs(Db, Docs, []).

save_docs(#db{server=Server}=Db, Docs, Options) ->
    Docs1 = [maybe_docid(Server, Doc) || Doc <- Docs],
    Options1 = couchbeam_util:parse_options(Options),
    {Options2, Body} = case proplists:get_value("all_or_nothing", 
            Options1, false) of
        true ->
            Body1 = couchbeam:json_encode({[
                {<<"all_or_nothing">>, true},
                {<<"docs">>, Docs1}
            ]}),

            {proplists:delete("all_or_nothing", Options1), Body1};
        _ ->
            Body1 = couchbeam_util:json_encode({[{<<"docs">>, Docs1}]}),
            {Options1, Body1}
        end,
    Url = make_url(Server, [db_url(Db), "/", "_bulk_docs"], Options2),
    Headers = [{"Content-Type", "application/json"}], 
    case db_request(post, Url, ["201"], Headers, Body) of
        {ok, _, _, RespBody} ->
            {ok, couchbeam_util:json_decode(RespBody)};
        Error -> 
            Error
        end.

all_docs(Db) ->
    all_docs(Db, []).
all_docs(Db, Options) ->
    view(Db, "_all_docs", Options).

view(Db, ViewName) ->
    view(Db, ViewName, []).

view(#db{server=Server}=Db, ViewName, Options) ->
    ViewName1 = couchbeam_util:to_list(ViewName),
    Options1 = couchbeam_util:parse_options(Options),
    Url = case ViewName1 of
        {DName, VName} ->
            [db_url(Db), "/_design/", DName, "/_view/", VName];
        "_all_docs" ->
            [db_url(Db), "/_all_docs"];
        _Else ->
            case string:tokens(ViewName1, "/") of
            [DName, VName] ->
                [db_url(Db), "/_design/", DName, "/_view/", VName];
            _ ->
                undefined
            end
    end,
    case Url of
    undefined ->
        {error, invalid_view_name};
    _ ->
        {Method, Options2, Body} = case proplists:get_value("keys",
                Options1) of
            undefined ->
                {get, Options1, []};
            Keys ->
                Body1 = couchbeam_util:json_encode({[{<<"keys">>, Keys}]}),
                {post, proplists:delete("keys", Options1), Body1}
            end,
        Headers = case Method of
            post -> [{"Content-Type", "application/json"}];
            _ -> []
        end,
        NewView = #view{
            db = Db,
            name = ViewName,
            options = Options2,
            method = Method,
            body = Body,
            headers = Headers,
            url_parts = Url,
            url = make_url(Server, Url, Options2)
        },
        {ok, NewView}
    end.


ensure_full_commit(Db) ->
    ensure_full_commit(Db, []).

ensure_full_commit(#db{server=Server}=Db, Options) ->
    Url = make_url(Server, [db_url(Db), "/_ensure_full_commit"], Options),
    Headers = [{"Content-Type", "application/json"}],
    case db_request(post, Url, ["201"], Headers) of
        {ok, _, _, Body} ->
            {[{<<"ok">>, true}|R]} = couchbeam_util:json_decode(Body),
            {ok, R};
        Error ->
            Error
    end.

compact(#db{server=Server}=Db) ->
    Url = make_url(Server, [db_url(Db), "/_compact"], []),
    Headers = [{"Content-Type", "application/json"}],
    case db_request(post, Url, ["202"], Headers) of
        {ok, _, _, _} ->
            ok;
        Error -> 
            Error
    end.

compact(#db{server=Server}=Db, DesignName) ->
    Url = make_url(Server, [db_url(Db), "/_compact/", DesignName], []),
    Headers = [{"Content-Type", "application/json"}],
    case db_request(post, Url, ["202"], Headers) of
        {ok, _, _, _} ->
            ok;
        Error -> 
            Error
    end.



%% --------------------------------------------------------------------
%% Utilities functins.
%% --------------------------------------------------------------------

%% add missing docid to a list of documents if needed
maybe_docid(Server, {DocProps}) ->
    case proplists:get_value(<<"_id">>, DocProps) of
        undefined ->
            DocId = [get_uuid(Server)],
            {[{<<"_id">>, list_to_binary(DocId)}|DocProps]};
        _DocId ->
            {DocProps}
    end.

%% @doc Assemble the server URL for the given client
%% @spec server_url({Host, Port}) -> iolist()
server_url(#server{host=Host, port=Port, ssl=Ssl}) ->
    server_url({Host, Port}, Ssl).

%% @doc Assemble the server URL for the given client
%% @spec server_url({Host, Port}, Ssl) -> iolist()
server_url({Host, Port}, false) ->
    ["http://",Host,":",integer_to_list(Port)];
server_url({Host, Port}, true) ->
    ["https://",Host,":",integer_to_list(Port)].

uuids_url(Server) ->
    binary_to_list(iolist_to_binary([server_url(Server), "/", "_uuids"])).

db_url(#db{name=DbName}) ->
    [DbName].

doc_url(Db, DocId) ->
    [db_url(Db), "/", DocId].

make_url(Server=#server{prefix=Prefix}, Path, Query) ->
    Query1 = encode_query(Query),
    binary_to_list(
        iolist_to_binary(
            [server_url(Server),
             Prefix, "/",
             Path, "/",
             [ ["?", mochiweb_util:urlencode(Query1)] || Query1 =/= [] ]
            ])).

%% @doc Encode needed value of Query proplists in json
encode_query([]) ->
    [];
encode_query(Query) when is_list(Query) ->
    lists:foldl(fun({K, V}, Acc) ->
        V1 = encode_query_value(K, V), 
        [{K, V1}|Acc]
    end, [], Query);
encode_query(Query) ->
    Query.

%% @doc Encode value in JSON if needed depending on the key 
encode_query_value(K, V) when is_atom(K) ->
    encode_query_value(atom_to_list(K), V);
encode_query_value(K, V) when is_binary(K) ->
    encode_query_value(binary_to_list(K), V);
encode_query_value(K, V) ->
    case K of
        "key" -> couchbeam_util:json_encode(V);
        "startkey" -> couchbeam_util:json_encode(V);
        "endkey" -> couchbeam_util:json_encode(V);
        _ -> V
    end.


db_request(Method, Url, Expect) ->
    db_request(Method, Url, Expect, [], []).
db_request(Method, Url, Expect, Headers) ->
    db_request(Method, Url, Expect, Headers, []).
db_request(Method, Url, Expect, Headers, Body) ->
    case request(Method, Url, Expect, Headers, Body) of
        Resp = {ok, _, _, _} ->
            Resp;
        {error, {ok, "404", _, _}} ->
            {error, not_found};
        {error, {ok, "409", _, _}} ->
            {error, conflict};
        {error, {ok, "412", _, _}} ->
            {error, precondition_failed};
        Error -> 
            Error
    end.

%% @doc send an ibrowse request
request(Method, Url, Expect) ->
    request(Method, Url, Expect, [], []).
request(Method, Url, Expect, Headers) ->
    request(Method, Url, Expect, Headers, []).
request(Method, Url, Expect, Headers, Body) ->
    Accept = {"Accept", "application/json, */*;q=0.9"},
    case ibrowse:send_req(Url, [Accept|Headers], Method, Body, 
            [{response_format, binary}]) of
        Resp={ok, Status, _, _} ->
            case lists:member(Status, Expect) of
                true -> Resp;
                false -> {error, Resp}
            end;
        Error -> Error
    end.

%% @doc stream an ibrowse request
request_stream(Pid, Method, Url) ->
    request_stream(Pid, Method, Url, []).
request_stream(Pid, Method, Url, Headers) ->
    request_stream(Pid, Method, Url, Headers, []).
request_stream(Pid, Method, Url, Headers, Body) ->
    case ibrowse:send_req(Url, Headers, Method, Body,
                          [{stream_to, Pid},
                           {response_format, binary}]) of
        {ibrowse_req_id, ReqId} ->
            {ok, ReqId};
        Error ->
            Error
    end.


%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------
%% @private

init(_) ->
    process_flag(trap_exit, true),
    ets:new(couchbeam_uuids, [named_table, public, {keypos, 2}]),
    {ok, #state{}}.

handle_call({get_uuids, #server{host=Host, port=Port}=Server, Count}, _From, State) ->
    {ok, Uuids} = do_get_uuids(Server, Count, [],
        ets:lookup(couchbeam_uuids, {Host, Port})),
    {reply, Uuids, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_get_uuids(_Server, Count, Acc, _) when length(Acc) >= Count ->
    {ok, Acc};
do_get_uuids(Server, Count, Acc, []) ->
    {ok, ServerUuids} = get_new_uuids(Server),
    do_get_uuids(Server, Count, Acc, [ServerUuids]);
do_get_uuids(Server, Count, Acc, [#server_uuids{uuids=Uuids}]) ->
    case Uuids of 
        [] ->
            {ok, ServerUuids} = get_new_uuids(Server),
            do_get_uuids(Server, Count, Acc, [ServerUuids]);
        _ ->
            {Acc1, Uuids1} = do_get_uuids1(Acc, Uuids, Count),
            #server{host=Host, port=Port} = Server,
            ServerUuids = #server_uuids{host_port={Host,Port},
                uuids=Uuids1},
            ets:insert(couchbeam_uuids, ServerUuids),
            do_get_uuids(Server, Count, Acc1, [ServerUuids])
    end.



do_get_uuids1(Acc, Uuids, 0) ->
    {Acc, Uuids};
do_get_uuids1(Acc, [Uuid|Rest], Count) ->
    do_get_uuids1([Uuid|Acc], Rest, Count-1).


get_new_uuids(Server=#server{host=Host, port=Port}) ->
    Url = make_url(Server, "_uuids", [{"count", "1000"}]),  
    case request(get, Url, ["200"]) of
        {ok, _Status, _Headers, Body} ->
            {[{<<"uuids">>, Uuids}]} = couchbeam_util:json_decode(Body),
            ServerUuids = #server_uuids{host_port={Host,
                        Port}, uuids=Uuids},
            ets:insert(couchbeam_uuids, ServerUuids),
            {ok, ServerUuids};
        Error ->
            Error
    end.



