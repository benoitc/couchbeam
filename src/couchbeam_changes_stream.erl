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

-export([init/1,
         handle_event/2,
         wait_results/2,
         wait_results1/2,
         collect_object/2,
         maybe_continue_decoding/1]).

-include("couchbeam.hrl").


-record(state, {parent,
                owner,
                ref,
                mref,
                db,
                options,
                client_ref=nil,
                decoder,
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

    %% register the stream
    ets:insert(couchbeam_changes_streams, [{StreamRef, self()}]),

    %% initialise the last sequece
    put(last_seq, Since),

    %% tell to the parent that we are ok
    proc_lib:init_ack(Parent, {ok, self()}),

    %% start the loop
    loop(State),
    %% stop to monitor the parent
    erlang:demonitor(MRef),
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
            Body =  couchbeam_ejson:encode({[{<<"doc_ids">>, DocIds}]}),
            Headers = [{<<"Content-Type">>, <<"application/json">>}],
            couchbeam_httpc:request(post, Url, Headers, Body, ConnOpts1)
    end,
    receive
        {'DOWN', MRef, _, _, _} ->
            %% parent exited there is no need to continue
            exit(normal);
        {hackney_response, ClientRef, {status, 200, _}} ->
            State1 = State#state{client_ref=ClientRef},
            DecoderFun = case FeedType of
                longpoll ->
                    jsx:decoder(?MODULE, [State1], [stream]);
                _ ->
                    nil
            end,
            {ok, State1#state{decoder=DecoderFun}};
        {hackney_response, ClientRef, {error, Reason}} ->
            exit(Reason)
    after ?TIMEOUT ->
           exit(timeout)
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
        {hackney_response, ClientRef, done} ->
            maybe_reconnect(State);
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
  Seq = couchbeam_util:get_value(<<"seq">>, Props),
  put(last_seq, Seq),
  Owner ! {Ref, {change, {Props}}}.

decode(Data) ->
  jsx:decode(Data,[return_tail,stream]).

decodefun(nil) ->
  fun(Data) -> decode(Data) end;
decodefun(Fun) ->
  Fun.

decode_with_tail(Data, Fun, State) ->
  case (decodefun(Fun))(Data) of
    {with_tail,Props,Rest} ->
      seq(Props,State),
      decode_with_tail(Rest,decodefun(nil),State);
    Other -> Other
  end.

decode_data(Data, #state{feed_type=continuous,
  decoder=DecodeFun}=State) ->

  {incomplete, DecodeFun2} =
    try
      decode_with_tail(Data,DecodeFun,State)
    catch error:badarg -> exit(badarg)
    end,

  try DecodeFun2(end_stream) of
    Props ->
      seq(Props,State),
      maybe_continue(State#state{decoder=nil})
  catch error:badarg -> maybe_continue(State#state{decoder=DecodeFun2})
  end;
decode_data(Data, #state{client_ref=ClientRef,
                         decoder=DecodeFun}=State) ->
    try
        {incomplete, DecodeFun2} = DecodeFun(Data),
        try DecodeFun2(end_stream) of done ->
            %% stop the request
            {ok, _} = hackney:stop_async(ClientRef),
            %% skip the rest of the body so the socket is
            %% replaced in the pool
            hackney:skip_body(ClientRef),
            %% maybe reconnect
            maybe_reconnect(State)
        catch error:badarg ->
            maybe_continue(State#state{decoder=DecodeFun2})
        end
    catch error:badarg -> exit(badarg)
    end.

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


%%% json decoder %%%

init([State]) ->
    {wait_results, 0, [[]], State}.


handle_event(end_json, _) ->
    done;
handle_event(Event, {Fun, _, _, _}=St) ->
    ?MODULE:Fun(Event, St).



wait_results(start_object, St) ->
    St;
wait_results(end_object, St) ->
    St;
wait_results({key, <<"results">>}, {_, _, _, St}) ->
    {wait_results1, 0, [[]], St};
wait_results(_,  {_, _, _, St}) ->
    {wait_results, 0, [[]], St}.



wait_results1(start_array, {_, _, _, St}) ->
    {wait_results1, 0, [[]], St};
wait_results1(start_object, {_, _, Terms, St}) ->
    {collect_object, 0, [[]|Terms], St};
wait_results1(end_array, {_, _, _, St}) ->
    {wait_results, 0, [[]], St}.


collect_object(start_object, {_, NestCount, Terms, St}) ->
    {collect_object, NestCount + 1, [[]|Terms], St};

collect_object(end_object, {_, NestCount, [[], {key, Key}, Last|Terms],
                           St}) ->
    {collect_object, NestCount - 1, [[{Key, {[{}]}}] ++ Last] ++ Terms,
     St};

collect_object(end_object, {_, NestCount, [Object, {key, Key},
                                           Last|Terms], St}) ->
    {collect_object, NestCount - 1,
     [[{Key, {lists:reverse(Object)}}] ++ Last] ++ Terms, St};

collect_object(end_object, {_, 0, [[], Last|Terms], St}) ->
    [[Change]] = [[{[{}]}] ++ Last] ++ Terms,
    send_change(Change, St);

collect_object(end_object, {_, NestCount, [[], Last|Terms], St}) ->
    {collect_object, NestCount - 1, [[{[{}]}] ++ Last] ++ Terms, St};

collect_object(end_object, {_, 0, [Object, Last|Terms], St}) ->
    [[Change]] = [[{lists:reverse(Object)}] ++ Last] ++ Terms,
    send_change(Change, St);


collect_object(end_object, {_, NestCount, [Object, Last|Terms], St}) ->
    Acc = [[{lists:reverse(Object)}] ++ Last] ++ Terms,
    {collect_object, NestCount - 1, Acc, St};


collect_object(start_array, {_, NestCount, Terms, St}) ->
    {collect_object, NestCount, [[]|Terms], St};
collect_object(end_array, {_, NestCount, [List, {key, Key}, Last|Terms],
                          St}) ->
    {collect_object, NestCount,
     [[{Key, lists:reverse(List)}] ++ Last] ++ Terms, St};
collect_object(end_array, {_, NestCount, [List, Last|Terms], St}) ->
    {collect_object, NestCount, [[lists:reverse(List)] ++ Last] ++ Terms,
     St};

collect_object({key, Key}, {_, NestCount, Terms, St}) ->
    {collect_object, NestCount, [{key, Key}] ++ Terms,
     St};

collect_object({_, Event}, {_, NestCount, [{key, Key}, Last|Terms], St}) ->
    {collect_object, NestCount, [[{Key, Event}] ++ Last] ++ Terms, St};
collect_object({_, Event}, {_, NestCount, [Last|Terms], St}) ->
    {collect_object, NestCount, [[Event] ++ Last] ++ Terms, St}.

send_change({Props}=Change, #state{owner=Owner, ref=Ref}=St) ->
    Seq = couchbeam_util:get_value(<<"seq">>, Props),
    put(last_seq, Seq),
    Owner ! {Ref, {change, Change}},
    maybe_continue_decoding(St).


%% eventually wait for the next call from the parent
maybe_continue_decoding(#state{parent=Parent,
                               owner=Owner,
                               ref=Ref,
                               mref=MRef,
                               client_ref=ClientRef,
                               async=once}=St) ->
    receive
        {'DOWN', MRef, _, _, _} ->
            %% parent exited there is no need to continue
            exit(normal);
        {Ref, stream_next} ->
            {wait_results1, 0, [[]], St};
        {Ref, cancel} ->
            hackney:close(ClientRef),
            %% unregister the stream
            ets:delete(couchbeam_changes_streams, Ref),
            %% tell the parent we exited
            Owner ! {Ref, ok},
            %% and exit
            exit(normal);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {maybe_continue_decoding, St});
        Else ->
            error_logger:error_msg("Unexpected message: ~w~n", [Else]),
            %% unregister the stream
            ets:delete(couchbeam_changes_streams, Ref),
            %% report the error
            report_error(Else, Ref, Owner),
            exit(Else)
    after 5000 ->
            erlang:hibernate(?MODULE, maybe_continue_decoding, [St])
    end;

maybe_continue_decoding(#state{parent=Parent,
                               owner=Owner,
                               ref=Ref,
                               mref=MRef,
                               client_ref=ClientRef}=St) ->
    receive
        {'DOWN', MRef, _, _, _} ->
            %% parent exited there is no need to continue
            exit(normal);
        {Ref, cancel} ->
            hackney:close(ClientRef),
            Owner ! {Ref, ok},
            exit(normal);
        {Ref, pause} ->
            erlang:hibernate(?MODULE, maybe_continue_decoding, [St]);
        {Ref, resume} ->
            {wait_results1, 0, [[]], St};
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {maybe_continue_decoding, St});
        Else ->
            error_logger:error_msg("Unexpected message: ~w~n", [Else]),
            report_error(Else, Ref, Owner),
            exit(Else)
    after 0 ->
        {wait_results1, 0, [[]], St}
    end.

%% @private

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

post_decode([{}]) ->
    {[]};
post_decode([{_Key, _Value} | _Rest] = PropList) ->
    {[ {Key, post_decode(Value)} || {Key, Value} <- PropList ]};
post_decode(List) when is_list(List) ->
    [ post_decode(Term) || Term <- List];
post_decode(Term) ->
    Term.
