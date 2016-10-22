%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_uuids).
-behaviour(gen_server).

-export([random/0,
         utc_random/0]).

-export([start_link/0]).
-export([get_uuids/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-include("couchbeam.hrl").

-define(START_RETRY, 200).
-define(MAX_RETRY, 10000).

-record(state, {}).

%% @doc return a random uuid
-spec random() -> binary().
random() ->
    list_to_binary(hackney_bstr:to_hex(crypto:strong_rand_bytes(16))).

%% @doc return a random uuid based on time
-spec utc_random() -> binary().
utc_random() ->
    utc_suffix(hackney_bstr:to_hex(crypto:strong_rand_bytes(9))).

%% @doc Get a list of uuids from the server
%% @spec get_uuids(server(), integer()) -> lists()
get_uuids(Server, Count) ->
    gen_server:call(?MODULE, {get_uuids, Server, Count}, infinity).

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
%% @doc Starts the couchbeam process linked to the calling process. Usually
%% invoked by the supervisor couchbeam_sup
%% @spec start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------
%% @private

init(_) ->
    process_flag(trap_exit, true),
    ets:new(couchbeam_uuids, [named_table, public, {keypos, 2}]),
    {ok, #state{}}.

handle_call({get_uuids, #server{url=Url}=Server, Count},
        _From, State) ->
    {ok, Uuids} = do_get_uuids(Server, Count, [],
                               ets:lookup(couchbeam_uuids, Url)),
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
        _ when length(Uuids) < Count ->
            {ok, ServerUuids} = get_new_uuids(Server, Uuids),
            do_get_uuids(Server, Count, Acc, [ServerUuids]);
        _ ->
            {Acc1, Uuids1} = do_get_uuids1(Acc, Uuids, Count),
            #server{url=Url} = Server,
            ServerUuids = #server_uuids{server_url=Url, uuids=Uuids1},
            ets:insert(couchbeam_uuids, ServerUuids),
            do_get_uuids(Server, Count, Acc1, [ServerUuids])
    end.



do_get_uuids1(Acc, Uuids, 0) ->
    {Acc, Uuids};
do_get_uuids1(Acc, [Uuid|Rest], Count) ->
    do_get_uuids1([Uuid|Acc], Rest, Count-1).


get_new_uuids(Server) ->
    get_new_uuids(Server, []).

get_new_uuids(Server, Acc) ->
    get_new_uuids(Server, ?START_RETRY, Acc).

get_new_uuids(#server{url=ServerUrl, options=Opts}=Server, Backoff, Acc) ->
    Count = list_to_binary(integer_to_list(1000 - length(Acc))),
    Url = hackney_url:make_url(ServerUrl, <<"/_uuids">>, [{<<"count">>, Count}]),
    case couchbeam_httpc:request(get, Url, [], <<>>, Opts) of
        {ok, 200, _, Ref} ->
            {ok, Body} = hackney:body(Ref),
            {[{<<"uuids">>, Uuids}]} = couchbeam_ejson:decode(Body),
            ServerUuids = #server_uuids{server_url=ServerUrl,
                                        uuids=(Acc ++ Uuids)},
            ets:insert(couchbeam_uuids, ServerUuids),
            {ok, ServerUuids};
        {ok, Status, Headers, Ref} ->
            {ok, Body} = hackney:body(Ref),
            {error, {bad_response, {Status, Headers, Body}}};
        {error, closed} ->
            wait_for_retry(Server, Backoff, Acc);
        {error, connect_timeout} ->
            wait_for_retry(Server, Backoff, Acc);
        Error ->
            Error
    end.

wait_for_retry(_S, ?MAX_RETRY, _A) ->
    {error, max_retry};
wait_for_retry(S, B, A) ->
    %% increment backoff
    B2 = min(B bsl 1, ?MAX_RETRY),
    timer:sleep(B2),
    get_new_uuids(S, B2, A).

utc_suffix(Suffix) ->
    Now = {_, _, Micro} = erlang:timestamp(),
    Nowish = calendar:now_to_universal_time(Now),
    Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
    Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
    list_to_binary(Prefix ++ Suffix).
