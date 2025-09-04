%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%

-module(couchbeam_changes_stream).

-export([start_link/4]).

-export([init_stream/5,
         maybe_continue/1,
         wait_reconnect/1,
         system_continue/3,
         system_terminate/4,
         system_code_change/4]).

%% no incremental JSON decoder exports; using buffered/line-based decode

-include("couchbeam.hrl").


-record(state, {parent,
                owner,
                ref,
                mref,
                db,
                options,
                client_ref=nil,
                buffer,
                feed_type=continuous,
                reconnect_after=1000,
                async=normal}).

-define(TIMEOUT, 10000).


start_link(Owner, StreamRef, Db, Options) ->
    proc_lib:start_link(?MODULE, init_stream, [self(), Owner, StreamRef,
                                               Db, Options]).


init_stream(Parent, Owner, StreamRef, Db, Options) ->
    %% clean options
    Options1 = parse_options(Options, []),

    %% reconnect option, time to wait before re.connecting when using a
    %% longpoll feed. Default is 1s.
    ReconnectAfter = proplists:get_value(reconnect_after, Options1, 1000),

    %% type of asynchronous request
    Async = proplists:get_value(async, Options, normal),

    %% feed type
    {FeedType, FinalOptions} = case proplists:get_value(feed, Options1) of
        undefined ->
            {continuous, [{feed, continuous} | Options1]};
        Type ->
            {Type, Options1}
    end,

    %% Get since
    Since = proplists:get_value(since, FinalOptions, 0),

    %% monitor the process receiving§ the messages
    MRef = erlang:monitor(process, Owner),

    %% initial state
    InitState = #state{parent=Parent,
                       owner=Owner,
                       ref=StreamRef,
                       mref=MRef,
                       db=Db,
                       options=FinalOptions,
                       feed_type=FeedType,
                       reconnect_after=ReconnectAfter,
                       async=Async},

    %% connect to the changes
    {ok, State} = do_init_stream(InitState),

    %% initialise the last sequece
    put(last_seq, Since),

    %% register the stream before notifying parent to avoid race condition
    ets:insert(couchbeam_changes_streams, [{StreamRef, self()}]),

    %% tell to the parent that we are ok
    proc_lib:init_ack(Parent, {ok, self()}),

    %% start the loop
    try
        loop(State)
    after
        %% Always clean up the monitor reference
        erlang:demonitor(MRef, [flush])
    end,
    ok.

do_init_stream(#state{mref=MRef,
                      db=Db,
                      options=Options,
                      feed_type=FeedType}=State) ->
    #db{server=Server, options=ConnOpts} = Db,
    %% we are doing the request asynchronously
    ConnOpts1 = [{async, once}, {recv_timeout, infinity}| ConnOpts],

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

    %% make the changes url
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server),
                               [couchbeam_httpc:db_url(Db), <<"_changes">>],
                               Options1),

    {ok, ClientRef} = case DocIds of
        [] ->
            couchbeam_httpc:request(get, Url, [], <<>>, ConnOpts1);
        DocIds ->
            Body =  couchbeam_ejson:encode(#{<<"doc_ids">> => DocIds}),
            Headers = [{<<"Content-Type">>, <<"application/json">>}],
            couchbeam_httpc:request(post, Url, Headers, Body, ConnOpts1)
    end,

    case {FeedType, proplists:get_value(since, Options, 0)} of
        {continuous, now} ->
            {ok, State#state{client_ref=ClientRef}};
        _ ->
            receive
                {'DOWN', MRef, _, _, _} ->
                    %% parent exited there is no need to continue
                    exit(normal);
                {hackney_response, ClientRef, {status, 200, _}} ->
                    State1 = State#state{client_ref=ClientRef},
                    {ok, State1};
                {hackney_response, ClientRef, {error, Reason}} ->
                    exit(Reason)
            after ?TIMEOUT ->
                    exit(timeout)
            end
    end.

loop(#state{owner=Owner,
            ref=StreamRef,
            mref=MRef,
            client_ref=ClientRef}=State) ->


    hackney:stream_next(ClientRef),
    receive
        {'DOWN', MRef, _, _, _} ->
            %% parent exited there is no need to continue
            exit(normal);
        {hackney_response, ClientRef, {headers, _Headers}} ->
            loop(State);
        {hackney_response, ClientRef, {status, 200, _V}} ->
            loop(State);
        {hackney_response, ClientRef, done} ->
            handle_done(State);
        {hackney_response, ClientRef, <<"\n">>} ->
             maybe_continue(State);
        {hackney_response, ClientRef, Data} when is_binary(Data) ->
            decode_data(Data, State);
        {hackney_response, ClientRef, Error} ->
            ets:delete(couchbeam_changes_streams, StreamRef),
            %% report the error
            report_error(Error, StreamRef, Owner),
            exit(Error)
    end.


maybe_reconnect(#state{ref=Ref,
                       options=Options,
                       feed_type=longpoll,
                       reconnect_after=After}=State)
        when is_integer(After) ->
    %% longpoll connections, we will restart after the delay

    %% update the state so we will restart on the last sequence
    LastSeq = get(last_seq),
    Options1 = couchbeam_util:force_param(since, LastSeq, Options),
    NState = State#state{options=Options1,
                         client_ref=nil},
    %% send the message in the interval
    erlang:send_after(After, self(), {Ref, reconnect}),
    %% hibernate the process, waiting on the reconnect message
    erlang:hibernate(?MODULE, wait_reconnect, [NState]);
maybe_reconnect(#state{owner=Owner, ref=StreamRef}) ->
    %% stop the change stream
    %% unregister the stream
    ets:delete(couchbeam_changes_streams, StreamRef),
    %% tell to the owner that we are done and exit,
    LastSeq = get(last_seq),
    Owner ! {StreamRef, {done, LastSeq}}.

%% wait to reconnect
wait_reconnect(#state{parent=Parent,
                      owner=Owner,
                      mref=MRef,
                      ref=Ref}=State) ->
    receive
        {'DOWN', MRef, _, _, _} ->
            %% parent exited there is no need to continue
            exit(normal);
        {Ref, cancel} ->
            maybe_close(State),
            %% unregister the stream
            ets:delete(couchbeam_changes_streams, Ref),
            %% tell the parent we exited
            Owner ! {Ref, ok};
        {Ref, reconnect} ->
            {ok, NState} = do_init_stream(State),
            loop(NState);
        {Ref, _} ->
            wait_reconnect(State);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {wait_reconnect, State});
        Else ->
            error_logger:error_msg("Unexpected message: ~w~n", [Else]),
            %% unregister the stream
            ets:delete(couchbeam_changes_streams, Ref),
            %% report the error
            report_error(Else, Ref, Owner),
            exit(Else)
    after 0 ->
            loop(State)
    end.


seq(Props,#state{owner=Owner,ref=Ref}) ->
  Seq = maps:get(<<"seq">>, Props, undefined),
  put(last_seq, Seq),
  Owner ! {Ref, {change, Props}}.

decode_data(Data, #state{feed_type=continuous, buffer=undefined}=State) ->
    decode_data(Data, State#state{buffer = <<>>});
decode_data(Data, #state{feed_type=continuous, buffer=Buf}=State) ->
    NewBuf = <<Buf/binary, Data/binary>>,
    %% split on newlines to get complete JSON objects per change
    Lines = binary:split(NewBuf, <<"\n">>, [global]),
    %% if last line is empty, keep empty remainder; else keep last as remainder
    {Complete, Remainder} =
        case lists:last(Lines) of
            <<>> -> {lists:sublist(Lines, 1, length(Lines)-1), <<>>};
            Last -> {lists:sublist(Lines, 1, length(Lines)-1), Last}
        end,
    lists:foreach(fun(Line) ->
                          case Line of
                              <<>> -> ok;
                              Bin ->
                                  try
                                      Props = couchbeam_ejson:decode(Bin),
                                      seq(Props, State)
                                  catch _:_ -> ok
                                  end
                          end
                  end, Complete),
    maybe_continue(State#state{buffer = Remainder});
decode_data(Data, #state{buffer=undefined}=State) ->
    decode_data(Data, State#state{buffer = <<>>});
decode_data(Data, #state{buffer=Buf}=State) ->
    %% longpoll or normal: accumulate buffer
    maybe_continue(State#state{buffer = <<Buf/binary, Data/binary>>}).

handle_done(#state{feed_type=continuous}=State) ->
    %% for continuous feeds, just reconnect/finish
    maybe_reconnect(State);
handle_done(#state{client_ref=ClientRef, buffer=Buf}=State) ->
    %% stop and decode longpoll body
    catch hackney:stop_async(ClientRef),
    catch hackney:skip_body(ClientRef),
    try
        Map = couchbeam_ejson:decode(case Buf of undefined -> <<>>; _ -> Buf end),
        Results = maps:get(<<"results">>, Map, []),
        lists:foreach(fun(Props) -> seq(Props, State) end, Results),
        maybe_reconnect(State)
    catch _:_ -> maybe_reconnect(State) end.

maybe_continue(#state{parent=Parent,
                      owner=Owner,
                      ref=Ref,
                      mref=MRef,
                      async=once}=State) ->

    receive
        {'DOWN', MRef, _, _, _} ->
            %% parent exited there is no need to continue
            exit(normal);
        {Ref, stream_next} ->
            loop(State);
        {Ref, cancel} ->
            maybe_close(State),
            %% unregister the stream
            ets:delete(couchbeam_changes_streams, Ref),
            %% tell the parent we exited
            Owner ! {Ref, ok};
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {loop, State});
        Else ->
            error_logger:error_msg("Unexpected message: ~w~n", [Else]),
            %% unregister the stream
            ets:delete(couchbeam_changes_streams, Ref),
            %% report the error
            report_error(Else, Ref, Owner),
            exit(Else)
    after 0 ->
            loop(State)
    end;
maybe_continue(#state{parent=Parent,
                      owner=Owner,
                      ref=Ref,
                      mref=MRef}=State) ->
    receive
        {'DOWN', MRef, _, _, _} ->
            %% parent exited there is no need to continue
            exit(normal);
        {Ref, cancel} ->
            maybe_close(State),
            %% unregister the stream
            ets:delete(couchbeam_changes_streams, Ref),
            %% tell the parent we exited
            Owner ! {Ref, ok};
        {Ref, pause} ->
            erlang:hibernate(?MODULE, maybe_continue, [State]);
        {Ref, resume} ->
            loop(State);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {loop, State});
        Else ->
            error_logger:error_msg("Unexpected message: ~w~n", [Else]),
            %% unregister the stream
            ets:delete(couchbeam_changes_streams, Ref),
            %% report the error
            report_error(Else, Ref, Owner),
            exit(Else)
    after 0 ->
            loop(State)
    end.

system_continue(_, _, {wait_reconnect, State}) ->
    wait_reconnect(State);
system_continue(_, _, {maybe_continue, State}) ->
    maybe_continue(State);
system_continue(_, _, {loop, State}) ->
    loop(State).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, #state{ref=StreamRef}) ->
    %% unregister the stream
    catch ets:delete(couchbeam_changes_streams, StreamRef),
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.


%%% removed legacy jsx-driven decoder code

%% internal

%% parse options to get feed type when it's not passed in a tuple
%% to support the old api.
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

%% report errors
report_error({error, _What}=Error, Ref, Pid) ->
    Pid ! {Ref, Error};
report_error(What, Ref, Pid) ->
    Pid ! {Ref, {error, What}}.

%% close the connection only if one is active
maybe_close(#state{client_ref=nil}) ->
    ok;
maybe_close(#state{client_ref=Ref}) ->
    hackney:close(Ref).

%% legacy jsx-driven post_decode removed; maps are used natively

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

stream_changes_longpoll_test() ->
    {ok, _} = application:ensure_all_started(couchbeam),
    Server = couchbeam:server_connection(),
    {ok, Db} = couchbeam:create_db(Server, <<"couchbeam_changes_stream_test">>),
    %% start longpoll stream
    {ok, Ref} = couchbeam_changes:follow(Db, [longpoll, {since, now}]),
    %% create a change
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"sc1">>}),
    %% receive change and validate map structure
    receive
        {Ref, {change, Change}} ->
            ?assertMatch(#{}, Change),
            ?assertEqual(<<"sc1">>, maps:get(<<"id">>, Change)),
            ok
    after 5000 ->
        ?assert(false)
    end.

-endif.
