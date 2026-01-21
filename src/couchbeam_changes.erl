%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% Simplified changes streaming using hackney's process-per-connection model.
%%% No supervisor or ETS tracking - spawns minimal linked process.

-module(couchbeam_changes).

-include("couchbeam.hrl").

%% Import json_stream_parse state record for pattern matching
-record(st, {phase, buf, i, in_string, escape, arr_depth, obj_depth, obj_start}).

-export([follow/1, follow/2, follow/3,
         cancel/1,
         stream_next/1,
         follow_once/1, follow_once/2]).

%% Internal exports for spawn
-export([init_stream/1]).

-record(state, {
    owner :: pid(),
    ref :: reference(),
    mref :: reference() | undefined,
    db :: db(),
    options :: list(),
    client_ref = nil :: pid() | nil,
    parser = nil :: term(),  %% json_stream_parse state or line buffer
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
-spec follow(Db::db(), Options::list()) -> {ok, reference()}.
follow(Db, Options) ->
    follow(Db, Options, self()).

%% @doc Start following changes, sending messages to specified pid
-spec follow(Db::db(), Options::list(), To::pid()) -> {ok, reference()}.
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
                    {ok, Ref} = follow(Db, FinalOptions),
                    collect_changes(Ref);
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
            Body = couchbeam_ejson:encode(#{<<"doc_ids">> => DocIds}),
            Headers = [{<<"Content-Type">>, <<"application/json">>}],
            couchbeam_httpc:request(post, Url, Headers, Body, ConnOpts1)
    end,

    case Result of
        {ok, ClientRef} ->
            %% Wait for initial status
            case {FeedType, proplists:get_value(since, Options, 0)} of
                {continuous, now} ->
                    {ok, State#state{parser = <<>>, client_ref = ClientRef}};
                _ ->
                    receive
                        {'DOWN', MRef, _, _, _} ->
                            exit(normal);
                        {hackney_response, ClientRef, {status, 200, _}} ->
                            Parser = case FeedType of
                                longpoll ->
                                    json_stream_parse:init();
                                _ ->
                                    <<>>  %% Line buffer for continuous
                            end,
                            {ok, State#state{client_ref = ClientRef,
                                             parser = Parser}};
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

%% Continuous mode: line-based parsing (each change is a JSON object on its own line)
decode_data(Data, #state{feed_type = continuous, parser = Buffer} = State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    {State1, Remaining} = parse_lines(NewBuffer, State),
    maybe_continue(State1#state{parser = Remaining});

%% Longpoll mode: use json_stream_parse for streaming
decode_data(Data, #state{feed_type = longpoll, parser = Parser,
                         client_ref = ClientRef} = State) ->
    {Changes, Parser1} = json_stream_parse:feed(Data, Parser),
    State1 = lists:foldl(fun send_change/2, State, Changes),
    case Parser1 of
        #st{phase = done} ->
            %% All results parsed
            catch hackney:stop_async(ClientRef),
            catch hackney:skip_body(ClientRef),
            maybe_reconnect(State1#state{parser = Parser1});
        _ ->
            maybe_continue(State1#state{parser = Parser1})
    end;

%% Normal mode (non-streaming): shouldn't get here, but handle gracefully
decode_data(_Data, State) ->
    maybe_continue(State).

%% Parse newline-delimited JSON objects for continuous feed
parse_lines(Buffer, State) ->
    case binary:split(Buffer, <<"\n">>) of
        [Line, Rest] when byte_size(Line) > 0 ->
            %% Got a complete line - parse it
            case catch couchbeam_ejson:decode(Line) of
                {'EXIT', _} ->
                    %% Invalid JSON, skip this line
                    parse_lines(Rest, State);
                Change when is_map(Change) ->
                    State1 = send_change(Change, State),
                    parse_lines(Rest, State1);
                _ ->
                    parse_lines(Rest, State)
            end;
        [<<>>, Rest] ->
            %% Empty line (heartbeat), continue
            parse_lines(Rest, State);
        [Incomplete] ->
            %% No complete line yet
            {State, Incomplete}
    end.

send_change(Change, #state{buffer_size = 0, owner = Owner, ref = Ref} = State) ->
    %% No buffering - send immediately
    Seq = maps:get(<<"seq">>, Change, State#state.last_seq),
    Owner ! {Ref, {change, Change}},
    State#state{last_seq = Seq};

send_change(Change, #state{buffer_size = N, buffer = Buffer,
                           owner = Owner, ref = Ref} = State) ->
    Seq = maps:get(<<"seq">>, Change, State#state.last_seq),
    NewBuffer = [Change | Buffer],
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
            Body = couchbeam_ejson:encode(#{<<"doc_ids">> => DocIds}),
            Headers = [{<<"Content-Type">>, <<"application/json">>}],
            couchbeam_httpc:db_request(post, Url, Headers, Body, ConnOptions,
                                       [200, 202])
    end,

    case Resp of
        {ok, _, _, ClientRef} ->
            Props = couchbeam_httpc:json_body(ClientRef),
            LastSeq = maps:get(<<"last_seq">>, Props),
            Changes = maps:get(<<"results">>, Props),
            {ok, LastSeq, Changes};
        Error ->
            Error
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test parse_lines helper
parse_lines_test() ->
    %% Create a minimal state for testing
    State = #state{
        owner = self(),
        ref = make_ref(),
        buffer_size = 0,
        buffer = [],
        last_seq = 0
    },

    %% Test single complete line
    Line1 = <<"{\"seq\":1,\"id\":\"doc1\",\"changes\":[{\"rev\":\"1-abc\"}]}\n">>,
    {State1, Remaining1} = parse_lines(Line1, State),
    ?assertEqual(<<>>, Remaining1),
    ?assertEqual(1, State1#state.last_seq),

    %% Test incomplete line (no newline)
    Incomplete = <<"{\"seq\":2,\"id\":\"doc2\"">>,
    {State2, Remaining2} = parse_lines(Incomplete, State),
    ?assertEqual(Incomplete, Remaining2),
    ?assertEqual(0, State2#state.last_seq),

    %% Test multiple lines
    MultiLine = <<"{\"seq\":1,\"id\":\"a\",\"changes\":[]}\n{\"seq\":2,\"id\":\"b\",\"changes\":[]}\n">>,
    {State3, Remaining3} = parse_lines(MultiLine, State),
    ?assertEqual(<<>>, Remaining3),
    ?assertEqual(2, State3#state.last_seq),

    %% Test empty lines (heartbeat)
    WithEmpty = <<"\n\n{\"seq\":3,\"id\":\"c\",\"changes\":[]}\n">>,
    {State4, _} = parse_lines(WithEmpty, State),
    ?assertEqual(3, State4#state.last_seq),
    ok.

%% Test follow_once with mocked response
follow_once_test() ->
    couchbeam_mocks:setup(),
    try
        {ok, _} = application:ensure_all_started(couchbeam),

        %% Mock db_request to return changes response
        meck:expect(couchbeam_httpc, db_request,
            fun(get, Url, _H, _B, _O, _E) ->
                UrlBin = iolist_to_binary(Url),
                case binary:match(UrlBin, <<"_changes">>) of
                    nomatch ->
                        {error, not_found};
                    _ ->
                        Ref = make_ref(),
                        meck:expect(hackney, body, fun(_) ->
                            Changes = [
                                #{<<"seq">> => 1, <<"id">> => <<"doc1">>, <<"changes">> => [#{<<"rev">> => <<"1-abc">>}]},
                                #{<<"seq">> => 2, <<"id">> => <<"doc2">>, <<"changes">> => [#{<<"rev">> => <<"1-def">>}]}
                            ],
                            Body = couchbeam_ejson:encode(#{
                                <<"results">> => Changes,
                                <<"last_seq">> => 2
                            }),
                            {ok, Body}
                        end),
                        {ok, 200, [], Ref}
                end
            end),

        Server = couchbeam:server_connection(),
        Db = #db{server = Server, name = <<"testdb">>, options = []},

        {ok, LastSeq, Changes} = follow_once(Db, [normal]),
        ?assertEqual(2, LastSeq),
        ?assertEqual(2, length(Changes)),

        [C1, C2] = Changes,
        ?assertEqual(<<"doc1">>, maps:get(<<"id">>, C1)),
        ?assertEqual(<<"doc2">>, maps:get(<<"id">>, C2)),
        ok
    after
        couchbeam_mocks:teardown()
    end.

%% Test option parsing
parse_options_test() ->
    %% Test continuous option
    Opts1 = parse_options([continuous], []),
    ?assertEqual(continuous, proplists:get_value(feed, Opts1)),

    %% Test longpoll option
    Opts2 = parse_options([longpoll], []),
    ?assertEqual(longpoll, proplists:get_value(feed, Opts2)),

    %% Test include_docs
    Opts3 = parse_options([include_docs], []),
    ?assertEqual(true, proplists:get_value(include_docs, Opts3)),

    %% Test combined options
    Opts4 = parse_options([continuous, include_docs, {since, 10}], []),
    ?assertEqual(continuous, proplists:get_value(feed, Opts4)),
    ?assertEqual(true, proplists:get_value(include_docs, Opts4)),
    ?assertEqual(10, proplists:get_value(since, Opts4)),
    ok.

-endif.

