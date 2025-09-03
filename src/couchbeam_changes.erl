%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_changes).

-include("couchbeam.hrl").

-export([follow/1, follow/2,
         cancel_stream/1,
         stream_next/1,
         follow_once/1, follow_once/2]).

-spec follow(Db::db()) -> {ok, StreamRef::atom()} | {error, term()}.
follow(Db) ->
    follow(Db, []).

%% @doc Stream changes to a pid
%%  <p>Db : a db record</p>
%%  <p>Client : pid  or callback where to send changes events where events are
%%  The pid receive these events:
%%  <dl>
%%      <dt>{change, StartRef, {done, Lastseq::integer()}</dt>
%%          <dd>Connection terminated or you got all changes</dd>
%%      <dt>{change, StartRef, Row :: ejson_object()}</dt>
%%          <dd>Line of change</dd>
%%      <dt>{error, LastSeq::integer(), Msg::term()}</dt>
%%          <dd>Got an error, connection is closed when an error
%%          happend.</dd>
%% </dl>
%%    LastSeq is the last sequence of changes.</p>
%% While the callbac could be like:
%%<pre>
%%      fun({done, LastSeq}) ->
%%          ok;
%%      fun({done, LastSeq}) ->
%%          ok;
%%      fun({done, LastSeq}) ->
%%          ok.</pre>
%% <p><pre>>Options :: changes_stream_options() [continuous
%%    | longpoll
%%    | normal
%%    | include_docs
%%    | {since, integer() | now}
%%    | {timeout, integer()}
%%    | heartbeat | {heartbeat, integer()}
%%    | {filter, string()} | {filter, string(), list({string(), string() | integer()})}
%%    | {view, string()},
%%    | {docids, list))},
%%    | {stream_to, pid()},
%%    | {async, once | normal}]</pre>
%%
%%   <ul>
%%      <li><code>continuous | longpoll | normal</code>: set the type of changes
%%          feed to get</li>
%%      <li><code>include_doc</code>: if you want to include the doc in the line of
%%          change</li>
%%      <li><code>{timeout, Timeout::integer()}</code>: timeout</li>
%%      <li><code>heartbeat | {heartbeat, Heartbeat::integer()}</code>: set couchdb
%%          to send a heartbeat to maintain connection open</li>
%%      <li><code>{filter, FilterName} | {filter, FilterName, Args::list({key,
%%          value})}</code>: set the filter to use with optional arguments</li>
%%      <li><code>{view, ViewName}</code>: use a view function as filter. Note
%%          that it requires to set filter special value <code>"_view"</code>
%%          to enable this feature.</li>
%%      <li>>`{stream_to, Pid}': the pid where the changes will be sent,
%%      by default the current pid. Used for continuous and longpoll
%%      connections</li>
%%   </ul></p>
%%
%% <p> Return {ok, StartRef, ChangesPid} or {error, Error}. Ref can be
%% used to disctint all changes from this pid. ChangesPid is the pid of
%% the changes loop process. Can be used to monitor it or kill it
%% when needed.</p>
-spec follow(Db::db(), Options::changes_options()) ->
    {ok, StreamRef::atom()} | {error, term()}.
follow(Db, Options) ->
    {To, Options1} = case proplists:get_value(stream_to, Options) of
        undefined ->
            {self(), Options};
        Pid ->
            {Pid, proplists:delete(stream_to, Options)}
    end,

    Ref = make_ref(),
    case supervisor:start_child(couchbeam_changes_sup, [To, Ref, Db,
                                                        Options1]) of
        {ok, _Pid} ->
            {ok, Ref};
        {error, _} = Error ->
            Error
    end.

-spec follow_once(Db::db())
    -> {ok, LastSeq::integer(), Changes::list()} | {error,term()}.
follow_once(Db) ->
    follow_once(Db, []).


%% @doc fetch all changes at once using a normal or longpoll
%% connections.
%%
%%  <p>Db : a db record</p>
%% <p><pre>Options :: changes_options() [
%%    | longpoll
%%    | normal
%%    | include_docs
%%    | {since, integer() | now}
%%    | {timeout, integer()}
%%    | heartbeat | {heartbeat, integer()}
%%    | {filter, string()}
%%    | {filter, string(), list({string(), string() | integer()})}
%%    | {docids, list()))},
%%    | {stream_to, pid()}
%%    ]</pre>
%%
%%   <ul>
%%      <li><code>longpoll | normal</code>: set the type of changes
%%          feed to get</li>
%%      <li><code>include_docs</code>: if you want to include the doc in the line of
%%          change</li>
%%      <li><code>{timeout, Timeout::integer()}</code>: timeout</li>
%%      <li><code>heartbeat | {heartbeat, Heartbeat::integer()}</code>: set couchdb
%%          to send a heartbeat to maintain connection open</li>
%%      <li><code>{filter, FilterName} | {filter, FilterName, Args::list({key,
%%          value})</code>: set the filter to use with optional arguments</li>
%%      <li><code>{view, ViewName}</code>: use a view function as filter. Note
%%          that it requires to set filter special value <code>"_view"</code>
%%          to enable this feature.</li>
%%   </ul></p>
%%
%% <p>Result: <code>{ok, LastSeq::integer(), Rows::list()}</code> or
%% <code>{error, LastSeq, Error}</code>. LastSeq is the last sequence of changes.</p>
-spec follow_once(Db::db(), Options::changes_options())
    -> {ok, LastSeq::integer(), Changes::list()} | {error,term()}.
follow_once(Db, Options) ->
    case parse_options_once(Options, []) of
        {error, _}=Error ->
            Error;
        Options1 ->
            FinalOptions = couchbeam_util:force_param(reconnect_after,
                                                      false, Options1),
            case proplists:get_value(feed, FinalOptions) of
                longpoll ->
                    case follow(Db, FinalOptions) of
                        {ok, Ref} ->
                            collect_changes(Ref);
                        {error, _} = Error ->
                            Error
                    end;
                _ ->
                    changes_request(Db, FinalOptions)
            end
    end.



cancel_stream(Ref) ->
    with_changes_stream(Ref, fun(Pid) ->
                case supervisor:terminate_child(couchbeam_view_sup, Pid) of
                    ok ->
                        case supervisor:delete_child(couchbeam_view_sup, Pid) of
                            ok ->ok;
                            {error, not_found} -> ok;
                            Error -> Error
                        end;
                    Error ->
                        Error
                end
        end).

stream_next(Ref) ->
    with_changes_stream(Ref, fun(Pid) ->
                Pid ! {Ref, stream_next}
        end).

%% @private
collect_changes(Ref) ->
    collect_changes(Ref, []).

collect_changes(Ref, Acc) ->
    receive
        {Ref, {done, LastSeq}} ->
            Changes = lists:reverse(Acc),
            {ok, LastSeq, Changes};
        {Ref, {change, Change}} ->
            collect_changes(Ref, [Change|Acc]);
        {Ref, Error} ->
            Error;
        Error ->
            Error
    end.


with_changes_stream(Ref, Fun) ->
    case ets:lookup(couchbeam_changes_streams, Ref) of
        [] ->
            {error, stream_undefined};
        [{Ref, Pid}] ->
            %% Check if the process is still alive to avoid race conditions
            case is_process_alive(Pid) of
                true ->
                    Fun(Pid);
                false ->
                    %% Clean up the stale entry
                    ets:delete(couchbeam_changes_streams, Ref),
                    {error, stream_undefined}
            end
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

changes_once_normal_test() ->
    {ok, _} = application:ensure_all_started(couchbeam),
    Server = couchbeam:server_connection(),
    {ok, Db} = couchbeam:create_db(Server, <<"couchbeam_changes_test">>),
    Docs = [#{<<"_id">> => <<"c1">>}, #{<<"_id">> => <<"c2">>}],
    couchbeam:save_docs(Db, Docs),
    couchbeam:ensure_full_commit(Db),
    {ok, _Seq, Changes} = follow_once(Db, [normal, {since, 0}]),
    Ids = [maps:get(<<"id">>, C) || C <- Changes],
    ?assert(lists:member(<<"c1">>, Ids)),
    ?assert(lists:member(<<"c2">>, Ids)),
    ok.

changes_follow_longpoll_test() ->
    {ok, _} = application:ensure_all_started(couchbeam),
    Server = couchbeam:server_connection(),
    {ok, Db} = couchbeam:create_db(Server, <<"couchbeam_changes_test2">>),
    {ok, Ref} = follow(Db, [longpoll, {since, now}]),
    %% trigger a change
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"lc1">>}),
    %% expect one change back
    receive
        {Ref, {change, Change}} ->
            ?assertMatch(#{}, Change),
            %% we can cancel after receiving
            couchbeam_changes:cancel_stream(Ref),
            ok
    after 5000 ->
            ?assert(false)
    end.

-endif.


changes_request(#db{server=Server, options=ConnOptions}=Db, Options) ->
    %% if we are filtering the changes using docids, send a POST request
    %% instead of a GET to make sure it will be accepted whatever the
    %% number of doc ids given.
    {DocIds, Options1} = case proplists:get_value(doc_ids, Options) of
        undefined ->
            {[], Options};
        [] ->
             {[], Options};
        Ids ->
            {Ids, proplists:delete(doc_ids, Options)}
    end,

    %% make url
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server),
                               [couchbeam_httpc:db_url(Db), <<"_changes">>],
                               Options1),

    %% do the request
    Resp = case DocIds of
        [] ->
            couchbeam_httpc:db_request(get, Url, [], <<>>, ConnOptions,
                                       [200, 202]);
        _ ->
            Body =  couchbeam_ejson:encode(#{<<"doc_ids">> => DocIds}),
            Headers = [{<<"Content-Type">>, <<"application/json">>}],
            couchbeam_httpc:db_request(post, Url, Headers, Body, ConnOptions,
                                       [200, 202])
    end,

    case Resp of
        {ok, _, _, Ref} ->
            Props = couchbeam_httpc:json_body(Ref),
            LastSeq = maps:get(<<"last_seq">>, Props),
            Changes = maps:get(<<"results">>, Props),
            {ok, LastSeq, Changes};
        Error ->
            Error
    end.

parse_options_once([], Acc) ->
    lists:reverse(Acc);
parse_options_once([normal | Rest], Acc) ->
    parse_options_once(Rest, couchbeam_util:force_param(feed, normal,
                                                        Acc));
parse_options_once([continuous | _Rest], _Acc) ->
    {error, {badarg, continuous}};
parse_options_once([longpoll | Rest], Acc) ->
    parse_options_once(Rest, couchbeam_util:force_param(feed, longpoll,
                                                        Acc));
parse_options_once([heartbeat | Rest], Acc) ->
    parse_options_once(Rest, couchbeam_util:force_param(heartbeat, true,
                                                        Acc));
parse_options_once([descending | Rest], Acc) ->
    parse_options_once(Rest, couchbeam_util:force_param(descending, true,
                                                        Acc));
parse_options_once([conflicts | Rest], Acc) ->
    parse_options_once(Rest, couchbeam_util:force_param(conflicts, true,
                                                        Acc));
parse_options_once([include_docs | Rest], Acc) ->
    parse_options_once(Rest, couchbeam_util:force_param(include_docs, true,
                                                        Acc));
parse_options_once([{K, V} | Rest], Acc) ->
    parse_options_once(Rest, [{K, V} | Acc]).
