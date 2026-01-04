%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Simplified changes streaming using hackney's process-per-connection model.
%%% No supervisor or ETS tracking - spawns minimal linked process.

-module(couchbeam_changes).

-include("couchbeam.hrl").

-export([follow/1, follow/2, follow/3,
         cancel/1,
         stream_next/1,
         follow_once/1, follow_once/2]).

%% Internal exports for spawn
-export([init_stream/1]).

%% JSX decoder callbacks (for longpoll mode)
-export([init/1, handle_event/2, wait_results/2, wait_results1/2,
         collect_object/2, maybe_continue_decoding/1]).

-record(state, {
    owner :: pid(),
    ref :: reference(),
    mref :: reference(),
    db :: db(),
    options :: list(),
    client_ref = nil :: pid() | nil,
    decoder = nil :: function() | nil,
    feed_type = continuous :: continuous | longpoll | normal,
    reconnect_after = 1000 :: integer() | false,
    async = normal :: once | normal,
    buffer_size = 0 :: non_neg_integer(),
    buffer = [] :: list(),
    last_seq = 0 :: term()
}).

-define(TIMEOUT, 10000).

%% @doc Start following changes on a database
-spec follow(Db::db()) -> {ok, reference()} | {error, term()}.
follow(Db) ->
    follow(Db, []).

%% @doc Start following changes on a database with options
-spec follow(Db::db(), Options::list()) -> {ok, reference()} | {error, term()}.
follow(Db, Options) ->
    follow(Db, Options, self()).

%% @doc Start following changes, sending messages to specified pid
-spec follow(Db::db(), Options::list(), To::pid()) -> {ok, reference()} | {error, term()}.
follow(Db, Options, To) ->
    Ref = make_ref(),
    Options1 = parse_options(Options, []),

    %% Extract settings
    FeedType = get_feed_type(Options1),
    AsyncMode = proplists:get_value(async, Options1, normal),
    BufferSize = proplists:get_value(buffer, Options1, 0),
    ReconnectAfter = proplists:get_value(reconnect_after, Options1,
        case FeedType of
            longpoll -> 1000;
            _ -> false
        end),
    Since = proplists:get_value(since, Options1, 0),

    %% Ensure feed type is in options
    FinalOptions = couchbeam_util:force_param(feed, FeedType, Options1),

    State = #state{
        owner = To,
        ref = Ref,
        db = Db,
        options = FinalOptions,
        feed_type = FeedType,
        async = AsyncMode,
        buffer_size = BufferSize,
        reconnect_after = ReconnectAfter,
        last_seq = Since
    },

    %% Spawn linked process - dies when owner dies
    Pid = spawn_link(?MODULE, init_stream, [State]),

    %% Store pid for cancel/stream_next (in caller's process dictionary)
    put({changes_stream, Ref}, Pid),

    {ok, Ref}.

%% @doc Cancel a changes stream
-spec cancel(Ref::reference()) -> ok | {error, term()}.
cancel(Ref) ->
    case get({changes_stream, Ref}) of
        undefined ->
            {error, stream_undefined};
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    Pid ! {Ref, cancel},
                    erase({changes_stream, Ref}),
                    ok;
                false ->
                    erase({changes_stream, Ref}),
                    {error, stream_undefined}
            end
    end.

%% @doc Request next chunk (for {async, once} mode)
-spec stream_next(Ref::reference()) -> ok | {error, term()}.
stream_next(Ref) ->
    case get({changes_stream, Ref}) of
        undefined ->
            {error, stream_undefined};
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    Pid ! {Ref, stream_next},
                    ok;
                false ->
                    erase({changes_stream, Ref}),
                    {error, stream_undefined}
            end
    end.

%% @doc Fetch all changes synchronously (non-streaming)
-spec follow_once(Db::db()) ->
    {ok, LastSeq::term(), Changes::list()} | {error, term()}.
follow_once(Db) ->
    follow_once(Db, []).

%% @doc Fetch all changes synchronously with options
-spec follow_once(Db::db(), Options::list()) ->
    {ok, LastSeq::term(), Changes::list()} | {error, term()}.
follow_once(Db, Options) ->
    case parse_options_once(Options, []) of
        {error, _} = Error ->
            Error;
        Options1 ->
            FinalOptions = couchbeam_util:force_param(reconnect_after, false, Options1),
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

%% ------------------------------------------------------------------
%% Internal: Stream process
%% ------------------------------------------------------------------

init_stream(#state{owner = Owner, ref = Ref} = State) ->
    %% Monitor the owner - exit if owner dies
    MRef = erlang:monitor(process, Owner),
    State1 = State#state{mref = MRef},

    case do_init_stream(State1) of
        {ok, State2} ->
            loop(State2);
        {error, Reason} ->
            Owner ! {Ref, {error, Reason}}
    end.

do_init_stream(#state{mref = MRef, db = Db, options = Options,
                      feed_type = FeedType} = State) ->
    #db{server = Server, options = ConnOpts} = Db,

    %% Async request with flow control
    ConnOpts1 = [{async, once}, {recv_timeout, infinity} | ConnOpts],

    %% Handle doc_ids for POST request
    {DocIds, Options1} = case proplists:get_value(doc_ids, Options) of
        undefined -> {[], Options};
        [] -> {[], Options};
        Ids -> {Ids, proplists:delete(doc_ids, Options)}
    end,

    %% Build URL
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server),
                               [couchbeam_httpc:db_url(Db), <<"_changes">>],
                               Options1),

    %% Make request
    Result = case DocIds of
        [] ->
            couchbeam_httpc:request(get, Url, [], <<>>, ConnOpts1);
        _ ->
            Body = couchbeam_ejson:encode({[{<<"doc_ids">>, DocIds}]}),
            Headers = [{<<"Content-Type">>, <<"application/json">>}],
            couchbeam_httpc:request(post, Url, Headers, Body, ConnOpts1)
    end,

    case Result of
        {ok, ClientRef} ->
            %% Wait for initial status
            case {FeedType, proplists:get_value(since, Options, 0)} of
                {continuous, now} ->
                    {ok, State#state{decoder = nil, client_ref = ClientRef}};
                _ ->
                    receive
                        {'DOWN', MRef, _, _, _} ->
                            exit(normal);
                        {hackney_response, ClientRef, {status, 200, _}} ->
                            DecoderFun = case FeedType of
                                longpoll ->
                                    jsx:decoder(?MODULE, [State], [stream]);
                                _ ->
                                    nil
                            end,
                            {ok, State#state{client_ref = ClientRef,
                                             decoder = DecoderFun}};
                        {hackney_response, ClientRef, {status, Status, Reason}} ->
                            {error, {http_error, Status, Reason}};
                        {hackney_response, ClientRef, {error, Reason}} ->
                            {error, Reason}
                    after ?TIMEOUT ->
                        {error, timeout}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

loop(#state{owner = Owner, ref = Ref, mref = MRef,
            client_ref = ClientRef} = State) ->
    hackney:stream_next(ClientRef),
    receive
        {'DOWN', MRef, _, _, _} ->
            %% Owner died, exit
            exit(normal);
        {hackney_response, ClientRef, {headers, _Headers}} ->
            loop(State);
        {hackney_response, ClientRef, {status, 200, _}} ->
            loop(State);
        {hackney_response, ClientRef, done} ->
            maybe_reconnect(State);
        {hackney_response, ClientRef, <<"\n">>} ->
            %% Heartbeat
            maybe_continue(State);
        {hackney_response, ClientRef, Data} when is_binary(Data) ->
            decode_data(Data, State);
        {hackney_response, ClientRef, {error, Reason}} ->
            Owner ! {Ref, {error, Reason}},
            exit(Reason);

        %% Control messages
        {Ref, stream_next} ->
            loop(State);
        {Ref, cancel} ->
            maybe_close(State),
            Owner ! {Ref, cancelled}
    end.

maybe_reconnect(#state{ref = Ref, options = Options, feed_type = longpoll,
                       reconnect_after = After, owner = Owner,
                       last_seq = LastSeq} = State) when is_integer(After) ->
    %% Flush buffer before reconnecting
    State1 = flush_buffer(State),

    %% Update since for reconnection
    Options1 = couchbeam_util:force_param(since, LastSeq, Options),
    State2 = State1#state{options = Options1, client_ref = nil},

    %% Notify owner
    Owner ! {Ref, reconnecting},

    %% Wait then reconnect
    erlang:send_after(After, self(), {Ref, reconnect}),
    wait_reconnect(State2);

maybe_reconnect(#state{owner = Owner, ref = Ref} = State) ->
    %% Flush remaining buffer and finish
    State1 = flush_buffer(State),
    Owner ! {Ref, {done, State1#state.last_seq}}.

wait_reconnect(#state{owner = Owner, ref = Ref, mref = MRef} = State) ->
    receive
        {'DOWN', MRef, _, _, _} ->
            exit(normal);
        {Ref, cancel} ->
            maybe_close(State),
            Owner ! {Ref, cancelled};
        {Ref, reconnect} ->
            case do_init_stream(State) of
                {ok, NewState} ->
                    loop(NewState);
                {error, Reason} ->
                    Owner ! {Ref, {error, Reason}}
            end;
        {Ref, _} ->
            wait_reconnect(State)
    end.

maybe_continue(#state{ref = Ref, mref = MRef, owner = Owner,
                      async = once} = State) ->
    receive
        {'DOWN', MRef, _, _, _} ->
            exit(normal);
        {Ref, stream_next} ->
            loop(State);
        {Ref, cancel} ->
            maybe_close(State),
            Owner ! {Ref, cancelled}
    after 0 ->
        loop(State)
    end;
maybe_continue(#state{ref = Ref, mref = MRef, owner = Owner} = State) ->
    receive
        {'DOWN', MRef, _, _, _} ->
            exit(normal);
        {Ref, cancel} ->
            maybe_close(State),
            Owner ! {Ref, cancelled};
        {Ref, pause} ->
            erlang:hibernate(?MODULE, maybe_continue, [State]);
        {Ref, resume} ->
            loop(State)
    after 0 ->
        loop(State)
    end.

%% ------------------------------------------------------------------
%% Internal: JSON decoding
%% ------------------------------------------------------------------

decode_data(Data, #state{feed_type = continuous, decoder = DecodeFun} = State) ->
    {incomplete, DecodeFun2} =
        try
            decode_with_tail(Data, DecodeFun, State)
        catch error:badarg ->
            maybe_close(State),
            exit(badarg)
        end,

    try DecodeFun2(end_stream) of
        Props ->
            State1 = send_change(Props, State),
            maybe_continue(State1#state{decoder = nil})
    catch error:badarg ->
        maybe_continue(State#state{decoder = DecodeFun2})
    end;

decode_data(Data, #state{client_ref = ClientRef, decoder = DecodeFun} = State) ->
    try
        {incomplete, DecodeFun2} = DecodeFun(Data),
        try DecodeFun2(end_stream) of
            done ->
                catch hackney:stop_async(ClientRef),
                catch hackney:skip_body(ClientRef),
                maybe_reconnect(State)
        catch error:badarg ->
            maybe_continue(State#state{decoder = DecodeFun2})
        end
    catch error:badarg ->
        maybe_close(State),
        exit(badarg)
    end.

decode(Data) ->
    jsx:decode(Data, [return_tail, stream]).

decodefun(nil) ->
    fun(D) -> decode(D) end;
decodefun(Fun) ->
    Fun.

decode_with_tail(Data, Fun, State) ->
    case (decodefun(Fun))(Data) of
        {with_tail, Props, Rest} ->
            State1 = send_change(Props, State),
            decode_with_tail(Rest, decodefun(nil), State1);
        Other ->
            Other
    end.

send_change(Props, #state{buffer_size = 0, owner = Owner, ref = Ref} = State) ->
    %% No buffering - send immediately
    Seq = couchbeam_util:get_value(<<"seq">>, Props),
    Owner ! {Ref, {change, {Props}}},
    State#state{last_seq = Seq};

send_change(Props, #state{buffer_size = N, buffer = Buffer,
                          owner = Owner, ref = Ref} = State) ->
    Seq = couchbeam_util:get_value(<<"seq">>, Props),
    NewBuffer = [{Props} | Buffer],
    case length(NewBuffer) >= N of
        true ->
            %% Buffer full - send batch
            Owner ! {Ref, {changes, lists:reverse(NewBuffer)}},
            State#state{buffer = [], last_seq = Seq};
        false ->
            %% Accumulate
            State#state{buffer = NewBuffer, last_seq = Seq}
    end.

flush_buffer(#state{buffer = [], owner = _Owner, ref = _Ref} = State) ->
    State;
flush_buffer(#state{buffer = Buffer, owner = Owner, ref = Ref} = State) ->
    Owner ! {Ref, {changes, lists:reverse(Buffer)}},
    State#state{buffer = []}.

maybe_close(#state{client_ref = nil}) ->
    ok;
maybe_close(#state{client_ref = ClientRef}) ->
    hackney:close(ClientRef).

%% ------------------------------------------------------------------
%% Internal: Option parsing
%% ------------------------------------------------------------------

get_feed_type(Options) ->
    case proplists:get_value(feed, Options) of
        undefined ->
            case proplists:get_bool(continuous, Options) of
                true -> continuous;
                false ->
                    case proplists:get_bool(longpoll, Options) of
                        true -> longpoll;
                        false -> continuous  % default
                    end
            end;
        Type ->
            Type
    end.

parse_options([], Acc) ->
    lists:reverse(Acc);
parse_options([normal | Rest], Acc) ->
    parse_options(Rest, couchbeam_util:force_param(feed, continuous, Acc));
parse_options([continuous | Rest], Acc) ->
    parse_options(Rest, couchbeam_util:force_param(feed, continuous, Acc));
parse_options([longpoll | Rest], Acc) ->
    parse_options(Rest, couchbeam_util:force_param(feed, longpoll, Acc));
parse_options([heartbeat | Rest], Acc) ->
    parse_options(Rest, couchbeam_util:force_param(heartbeat, true, Acc));
parse_options([descending | Rest], Acc) ->
    parse_options(Rest, couchbeam_util:force_param(descending, true, Acc));
parse_options([conflicts | Rest], Acc) ->
    parse_options(Rest, couchbeam_util:force_param(conflicts, true, Acc));
parse_options([include_docs | Rest], Acc) ->
    parse_options(Rest, couchbeam_util:force_param(include_docs, true, Acc));
parse_options([{K, V} | Rest], Acc) ->
    parse_options(Rest, [{K, V} | Acc]).

parse_options_once([], Acc) ->
    lists:reverse(Acc);
parse_options_once([normal | Rest], Acc) ->
    parse_options_once(Rest, couchbeam_util:force_param(feed, normal, Acc));
parse_options_once([continuous | _Rest], _Acc) ->
    {error, {badarg, continuous}};
parse_options_once([longpoll | Rest], Acc) ->
    parse_options_once(Rest, couchbeam_util:force_param(feed, longpoll, Acc));
parse_options_once([heartbeat | Rest], Acc) ->
    parse_options_once(Rest, couchbeam_util:force_param(heartbeat, true, Acc));
parse_options_once([descending | Rest], Acc) ->
    parse_options_once(Rest, couchbeam_util:force_param(descending, true, Acc));
parse_options_once([conflicts | Rest], Acc) ->
    parse_options_once(Rest, couchbeam_util:force_param(conflicts, true, Acc));
parse_options_once([include_docs | Rest], Acc) ->
    parse_options_once(Rest, couchbeam_util:force_param(include_docs, true, Acc));
parse_options_once([{K, V} | Rest], Acc) ->
    parse_options_once(Rest, [{K, V} | Acc]).

%% ------------------------------------------------------------------
%% Internal: Synchronous request
%% ------------------------------------------------------------------

collect_changes(Ref) ->
    collect_changes(Ref, []).

collect_changes(Ref, Acc) ->
    receive
        {Ref, {done, LastSeq}} ->
            Changes = lists:reverse(Acc),
            {ok, LastSeq, Changes};
        {Ref, {change, Change}} ->
            collect_changes(Ref, [Change | Acc]);
        {Ref, {changes, Changes}} ->
            collect_changes(Ref, lists:reverse(Changes) ++ Acc);
        {Ref, {error, _} = Error} ->
            Error;
        {Ref, reconnecting} ->
            %% For follow_once with longpoll, we don't reconnect
            collect_changes(Ref, Acc)
    end.

changes_request(#db{server = Server, options = ConnOptions} = Db, Options) ->
    {DocIds, Options1} = case proplists:get_value(doc_ids, Options) of
        undefined -> {[], Options};
        [] -> {[], Options};
        Ids -> {Ids, proplists:delete(doc_ids, Options)}
    end,

    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server),
                               [couchbeam_httpc:db_url(Db), <<"_changes">>],
                               Options1),

    Resp = case DocIds of
        [] ->
            couchbeam_httpc:db_request(get, Url, [], <<>>, ConnOptions,
                                       [200, 202]);
        _ ->
            Body = couchbeam_ejson:encode({[{<<"doc_ids">>, DocIds}]}),
            Headers = [{<<"Content-Type">>, <<"application/json">>}],
            couchbeam_httpc:db_request(post, Url, Headers, Body, ConnOptions,
                                       [200, 202])
    end,

    case Resp of
        {ok, _, _, ClientRef} ->
            {Props} = couchbeam_httpc:json_body(ClientRef),
            LastSeq = couchbeam_util:get_value(<<"last_seq">>, Props),
            Changes = couchbeam_util:get_value(<<"results">>, Props),
            {ok, LastSeq, Changes};
        Error ->
            Error
    end.

%% ------------------------------------------------------------------
%% JSX decoder callbacks (for longpoll mode)
%% ------------------------------------------------------------------

init([State]) ->
    {wait_results, 0, [[]], State}.

handle_event(end_json, _) ->
    done;
handle_event(Event, {Fun, _, _, _} = St) ->
    ?MODULE:Fun(Event, St).

wait_results(start_object, St) ->
    St;
wait_results(end_object, St) ->
    St;
wait_results({key, <<"results">>}, {_, _, _, St}) ->
    {wait_results1, 0, [[]], St};
wait_results(_, {_, _, _, St}) ->
    {wait_results, 0, [[]], St}.

wait_results1(start_array, {_, _, _, St}) ->
    {wait_results1, 0, [[]], St};
wait_results1(start_object, {_, _, Terms, St}) ->
    {collect_object, 0, [[] | Terms], St};
wait_results1(end_array, {_, _, _, St}) ->
    {wait_results, 0, [[]], St}.

collect_object(start_object, {_, NestCount, Terms, St}) ->
    {collect_object, NestCount + 1, [[] | Terms], St};

collect_object(end_object, {_, NestCount, [[], {key, Key}, Last | Terms], St}) ->
    {collect_object, NestCount - 1, [[{Key, {[{}]}}] ++ Last] ++ Terms, St};

collect_object(end_object, {_, NestCount, [Object, {key, Key}, Last | Terms], St}) ->
    {collect_object, NestCount - 1,
     [[{Key, {lists:reverse(Object)}}] ++ Last] ++ Terms, St};

collect_object(end_object, {_, 0, [[], Last | Terms], St}) ->
    [[Change]] = [[{[{}]}] ++ Last] ++ Terms,
    send_change_longpoll(Change, St);

collect_object(end_object, {_, NestCount, [[], Last | Terms], St}) ->
    {collect_object, NestCount - 1, [[{[{}]}] ++ Last] ++ Terms, St};

collect_object(end_object, {_, 0, [Object, Last | Terms], St}) ->
    [[Change]] = [[{lists:reverse(Object)}] ++ Last] ++ Terms,
    send_change_longpoll(Change, St);

collect_object(end_object, {_, NestCount, [Object, Last | Terms], St}) ->
    Acc = [[{lists:reverse(Object)}] ++ Last] ++ Terms,
    {collect_object, NestCount - 1, Acc, St};

collect_object(start_array, {_, NestCount, Terms, St}) ->
    {collect_object, NestCount, [[] | Terms], St};

collect_object(end_array, {_, NestCount, [List, {key, Key}, Last | Terms], St}) ->
    {collect_object, NestCount,
     [[{Key, lists:reverse(List)}] ++ Last] ++ Terms, St};

collect_object(end_array, {_, NestCount, [List, Last | Terms], St}) ->
    {collect_object, NestCount, [[lists:reverse(List)] ++ Last] ++ Terms, St};

collect_object({key, Key}, {_, NestCount, Terms, St}) ->
    {collect_object, NestCount, [{key, Key}] ++ Terms, St};

collect_object({_, Event}, {_, NestCount, [{key, Key}, Last | Terms], St}) ->
    {collect_object, NestCount, [[{Key, Event}] ++ Last] ++ Terms, St};

collect_object({_, Event}, {_, NestCount, [Last | Terms], St}) ->
    {collect_object, NestCount, [[Event] ++ Last] ++ Terms, St}.

send_change_longpoll({Props} = Change, #state{owner = Owner, ref = Ref} = St) ->
    Seq = couchbeam_util:get_value(<<"seq">>, Props),
    Owner ! {Ref, {change, Change}},
    maybe_continue_decoding(St#state{last_seq = Seq}).

maybe_continue_decoding(#state{ref = Ref, mref = MRef, owner = Owner,
                               client_ref = ClientRef, async = once} = St) ->
    receive
        {'DOWN', MRef, _, _, _} ->
            exit(normal);
        {Ref, stream_next} ->
            {wait_results1, 0, [[]], St};
        {Ref, cancel} ->
            hackney:close(ClientRef),
            Owner ! {Ref, cancelled},
            exit(normal)
    after 5000 ->
        erlang:hibernate(?MODULE, maybe_continue_decoding, [St])
    end;

maybe_continue_decoding(#state{ref = Ref, mref = MRef, owner = Owner,
                               client_ref = ClientRef} = St) ->
    receive
        {'DOWN', MRef, _, _, _} ->
            exit(normal);
        {Ref, cancel} ->
            hackney:close(ClientRef),
            Owner ! {Ref, cancelled},
            exit(normal);
        {Ref, pause} ->
            erlang:hibernate(?MODULE, maybe_continue_decoding, [St]);
        {Ref, resume} ->
            {wait_results1, 0, [[]], St}
    after 0 ->
        {wait_results1, 0, [[]], St}
    end.
