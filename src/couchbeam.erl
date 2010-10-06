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
-export([server_url/1, uuids_url/1, make_url/3]).

%% API urls
-export([server_connection/0, server_connection/2, server_connection/3,
        server_connection/5, server_connection/6, server_info/1]).

%% --------------------------------------------------------------------
%% Generic utilities.
%% --------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
%% @doc Starts the couchbeam process linked to the calling process. Usually invoked by the supervisor couchbeam_sup
%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Starts the couchbeam process without linking. Useful when testing using the shell
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the couchbeam process. Useful when testing using the shell.
stop() ->
    gen_server:call(couchbeam, stop).

 
%% @spec () -> Version
%%     Version = string()
%% @doc Return the version of the application.
version() ->
    {ok, Version} = application:get_key(couchbeam, vsn),
    Version.   

%% --------------------------------------------------------------------
%% API functins.
%% --------------------------------------------------------------------
server_connection() ->
    #server{host="127.0.0.1", port=5984, ssl=false, prefix="",
        id="default", options=[]}.

server_connection(Host, Port) ->
    server_connection(Host, Port, {Host, Port}).

server_connection(Host, Port, Id) ->
    server_connection(Host, Port, Id, "", []).


server_connection(Host, Port, Id, Prefix, Options) when is_integer(Port), Port =:=443 ->
    server_connection(Host, Port, Id, Prefix, Options, true);
server_connection(Host, Port, Id, Prefix, Options) ->
    server_connection(Host, Port, Id, Prefix, Options, false).



server_connection(Host, Port, Id, Prefix, Options, Ssl) when is_binary(Port) ->
    server_connection(Host, binary_to_list(Port), Id, Prefix, Options, Ssl);
server_connection(Host, Port, Id, Prefix, Options, Ssl) when is_list(Port) ->
    server_connection(Host, list_to_integer(Port), Id, Prefix, Options, Ssl); 
server_connection(Host, Port, Id, Prefix, Options, Ssl) ->
    #server{host=Host, port=Port, ssl=Ssl, prefix=Prefix,
        options=Options, id=Id}.


server_info(Server) ->
    Url = binary_to_list(iolist_to_binary(server_url(Server))),
    case request(get, Url, ["200"]) of
        {ok, _Status, _Headers, Body} ->
            Version = couchbeam_util:json_decode(Body),
            {ok, Version};
        Error -> Error
    end.

%% --------------------------------------------------------------------
%% Utilities functins.
%% --------------------------------------------------------------------



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
    binary_to_list(iolist_to_binary([server_url(Server), "_uuids"])).

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


%% @doc send an ibrowse request
request(Method, Url, Expect) ->
    request(Method, Url, Expect, [], []).
request(Method, Url, Expect, Headers) ->
    request(Method, Url, Expect, Headers, []).
request(Method, Url, Expect, Headers, Body) ->
    case ibrowse:send_req(Url, Headers, Method, Body, 
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
    ets:new(couchbeam_servers, [named_table, ordered_set, public]),
    ets:new(couchbeam_uuids, [named_table, ordered_set, public]),
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
do_get_uuids(_Server, Count, Acc, _) when size(Acc) >= Count ->
    Acc;
do_get_uuids(Server=#server{host=Host, port=Port}, Count, Acc, []) ->
    {ok, ServerUuids} = get_new_uuids(Server),
    do_get_uuids(Server, Count, Acc, ServerUuids);
do_get_uuids(Server, Count, Acc, [#server_uuids{host_port=HostPort, uuids=Uuids}=ServerUuids]) ->
    case Uuids of 
        [] ->
            {ok, ServerUuids} = get_new_uuids(Server),
            do_get_uuids(Server, Count, Acc, ServerUuids);
        _ ->
            {Acc1, Uuids1} = get_uuids(Acc, Uuids, Count),
            ServerUuids = #server_uuids{host_port=HostPort,
                uuids=Uuids1},
            ets:insert(couchbeam_uuids, ServerUuids),
            do_get_uuids(Server, Count, Acc1, ServerUuids)
    end.



get_uuids(Acc, Uuids, 0) ->
    {Acc, Uuids};
get_uuids(Acc, [Uuid|Rest], Count) ->
    get_uuids([Uuid|Acc], Rest, Count-1).


get_new_uuids(Server=#server{host=Host, port=Port}) ->
    case request(get, uuids_url(Server), ["200"]) of
        {ok, _Status, _Headers, Body} ->
            {[{<<"uuids">>, Uuids}]} = couchbeam_util:decode(Body),
            ServerUuids = #server_uuids{host_port={Host,
                        Port}, uuids=Uuids},
            ets:insert(couchbeam_uuids, ServerUuids),
            {ok, ServerUuids};
        Error ->
            Error
    end.



