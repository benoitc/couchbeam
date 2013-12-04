%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_view_stream).

-export([start_link/4]).

-export([init_stream/5,
         maybe_continue/1,
         system_continue/3,
         system_terminate/4,
         system_code_change/4]).


-export([init/1,
         handle_event/2,
         wait_rows/2,
         wait_rows1/2,
         wait_val/2,
         collect_object/2,
         maybe_continue_decoding/1]).


-include("couchbeam.hrl").

-record(state, {parent,
                owner,
                req,
                ref,
                client_ref=nil,
                decoder,
                async=normal}).

-record(viewst, {parent,
                 owner,
                 ref,
                 client_ref,
                 async=false}).


-define(TIMEOUT, 10000).
-define(DEFAULT_CHANGES_POLL, 5000). % we check every 5secs



start_link(Owner, StreamRef, {Db, Url, Args}, StreamOptions) ->
    proc_lib:start_link(?MODULE, init_stream, [self(), Owner, StreamRef,
                                               {Db, Url, Args},
                                               StreamOptions]).

init_stream(Parent, Owner, StreamRef, {_Db, _Url, _Args}=Req,
            StreamOptions) ->


    Async = proplists:get_value(async, StreamOptions, normal),

    InitState = #state{parent=Parent,
                       owner=Owner,
                       req=Req,
                       ref=StreamRef,
                       async=Async},

    %% connect to the view
    {ok, State} = do_init_stream(Req, InitState),

    %% register the stream
    ets:insert(couchbeam_view_streams, [{StreamRef, self()}]),

    %% tell to the parent that we are ok
    proc_lib:init_ack(Parent, {ok, self()}),

    %% start the loop
    loop(State).

do_init_stream({#db{options=Opts}, Url, Args}, State) ->
    %% we are doing the request asynchronously
    FinalOpts = [{async, once} | Opts],
    {ok, Ref} = case Args#view_query_args.method of
        get ->
            hackney:request(get, Url, [], <<>>, FinalOpts);
        post ->
            Body = couchbeam_ejson:encode({[{<<"keys">>,
                                             Args#view_query_args.keys}]}),
            hackney:request(post, Url, [], Body, FinalOpts)
    end,
    io:format("received ~n", []),
    receive
        {Ref, {status, 200, _}} ->
            #state{parent=Parent,
                   owner=Owner,
                   ref=StreamRef,
                   async=Async} = State,

            DecoderFun = jsx:decoder(?MODULE, [Parent, Owner, StreamRef,
                                               Ref, Async], [explicit_end]),
            {ok, State#state{client_ref=Ref,
                             decoder=DecoderFun}};

        {error, Reason} ->
            exit(Reason)
    after ?TIMEOUT ->
            exit(timeout)
    end.


loop(#state{owner=Owner,
            ref=StreamRef,
            client_ref=ClientRef,
            decoder=DecodeFun}=State) ->


    hackney:stream_next(ClientRef),
    receive
        {ClientRef, {headers, _Headers}} ->
            loop(State);
        {ClientRef, done} ->
            %% unregister the stream
            ets:delete(couchbeam_view_streams, StreamRef),
            %% tell to the owner that we are done and exit,
            Owner ! {StreamRef, done};
        {ClientRef, Data} when is_binary(Data) ->
            decode_data(DecodeFun, Data, State);
        {ClientRef, Error} ->
            ets:delete(couchbeam_view_streams, StreamRef),
            %% report the error
            report_error(Error, StreamRef, Owner),
            exit(Error)
    end.

decode_data(DecodeFun, Data, #state{owner=Owner,
                                    ref=StreamRef,
                                    client_ref=ClientRef}=State) ->
    case DecodeFun(Data) of
        {incomplete, DecodeFun2} ->
            maybe_continue(State#state{decoder=DecodeFun2});
        badarg ->
            exit(badarg);
        done ->
            %% stop the request
            ok = hackney:stop_async(ClientRef),
            %% skip the rest of the body so the socket is
            %% replaced in the pool
            hackney:skip_body(ClientRef),
            %% unregister the stream
            ets:delete(couchbeam_view_streams, StreamRef),
            %% tell to the owner that we are done and exit,
            Owner ! {StreamRef, done}
    end.


maybe_continue(#state{parent=Parent, owner=Owner, ref=Ref,
                      async=once}=State) ->

    receive
        {Ref, stream_next} ->
            loop(State);
        {Ref, cancel} ->
            maybe_close(State),
            %% unregister the stream
            ets:delete(couchbeam_view_streams, Ref),
            %% tell the parent we exited
            Owner ! {Ref, ok};
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {loop, State});
        Else ->
            error_logger:error_msg("Unexpected message: ~w~n", [Else]),
            %% unregister the stream
            ets:delete(couchbeam_view_streams, Ref),
            %% report the error
            report_error(Else, Ref, Owner),
            exit(Else)
    after 5000 ->
            erlang:hibernate(?MODULE, maybe_continue, [State])
    end;
maybe_continue(#state{parent=Parent, owner=Owner, ref=Ref}=State) ->
    receive
        {Ref, cancel} ->
            maybe_close(State),
            %% unregister the stream
            ets:delete(couchbeam_view_streams, Ref),
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
            ets:delete(couchbeam_view_streams, Ref),
            %% report the error
            report_error(Else, Ref, Owner),
            exit(Else)
    after 0 ->
            loop(State)
    end.

maybe_close(#state{client_ref=nil}) ->
    ok;
maybe_close(#state{client_ref=Ref}) ->
    hackney:close(Ref).


system_continue(_, _, {maybe_continue, State}) ->
    maybe_continue(State);
system_continue(_, _, {loop, State}) ->
    loop(State).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, #state{ref=StreamRef}) ->
    %% unregister the stream
    catch ets:delete(couchbeam_view_streams, StreamRef),
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.


%%% json decoder %%%

init([Parent, Owner, StreamRef, ClientRef, Async]) ->
    InitialState = #viewst{parent=Parent,
                           owner=Owner,
                           ref=StreamRef,
                           client_ref=ClientRef,
                           async=Async},
    {wait_rows, 0, [[]], InitialState}.

handle_event(end_json, _) ->
    done;
handle_event(Event, {Fun, _, _, _}=St) ->
    ?MODULE:Fun(Event, St).



wait_rows(start_object, St) ->
    St;
wait_rows(end_object, St) ->
    St;
wait_rows({key, <<"rows">>}, {_, _, _, ViewSt}) ->
    {wait_rows1, 0, [[]], ViewSt};
wait_rows({key, <<"total_rows">>},  {_, _, _, ViewSt}) ->
    {wait_val, 0, [[]], ViewSt};
wait_rows({key, <<"offset">>},  {_, _, _, ViewSt}) ->
    {wait_val, 0, [[]], ViewSt}.

wait_val({_, _}, {_, _, _, ViewSt}) ->
    {wait_rows, 0, [[]], ViewSt}.

wait_rows1(start_array, {_, _, _, ViewSt}) ->
    {wait_rows1, 0, [[]], ViewSt};
wait_rows1(start_object, {_, _, Terms, ViewSt}) ->
    {collect_object, 0, [[]|Terms], ViewSt};
wait_rows1(end_array, {_, _, _, ViewSt}) ->
    {wait_rows, 0, [[]], ViewSt}.


collect_object(start_object, {_, NestCount, Terms, ViewSt}) ->
    {collect_object, NestCount + 1, [[]|Terms], ViewSt};

collect_object(end_object, {_, NestCount, [[], {key, Key}, Last|Terms],
                           ViewSt}) ->
    {collect_object, NestCount - 1, [[{Key, {[{}]}}] ++ Last] ++ Terms,
     ViewSt};

collect_object(end_object, {_, NestCount, [Object, {key, Key},
                                           Last|Terms], ViewSt}) ->
    {collect_object, NestCount - 1,
     [[{Key, {lists:reverse(Object)}}] ++ Last] ++ Terms, ViewSt};

collect_object(end_object, {_, 0, [[], Last|Terms], ViewSt}) ->
    [[Row]] = [[{[{}]}] ++ Last] ++ Terms,
    send_row({Row}, ViewSt);

collect_object(end_object, {_, NestCount, [[], Last|Terms], ViewSt}) ->
    {collect_object, NestCount - 1, [[{[{}]}] ++ Last] ++ Terms, ViewSt};

collect_object(end_object, {_, 0, [Object, Last|Terms], ViewSt}) ->
    [[Row]] = [[{lists:reverse(Object)}] ++ Last] ++ Terms,
    send_row(Row, ViewSt);


collect_object(end_object, {_, NestCount, [Object, Last|Terms], ViewSt}) ->
    Acc = [[{lists:reverse(Object)}] ++ Last] ++ Terms,
    {collect_object, NestCount - 1, Acc, ViewSt};


collect_object(start_array, {_, NestCount, Terms, ViewSt}) ->
    {collect_object, NestCount, [[]|Terms], ViewSt};
collect_object(end_array, {_, NestCount, [List, {key, Key}, Last|Terms],
                          ViewSt}) ->
    {collect_object, NestCount,
     [[{Key, lists:reverse(List)}] ++ Last] ++ Terms, ViewSt};
collect_object(end_array, {_, NestCount, [List, Last|Terms], ViewSt}) ->
    {collect_object, NestCount, [[lists:reverse(List)] ++ Last] ++ Terms,
     ViewSt};

collect_object({key, Key}, {_, NestCount, Terms, ViewSt}) ->
    {collect_object, NestCount, [{key, Key}] ++ Terms,
     ViewSt};

collect_object({_, Event}, {_, NestCount, [{key, Key}, Last|Terms], ViewSt}) ->
    {collect_object, NestCount, [[{Key, Event}] ++ Last] ++ Terms, ViewSt};
collect_object({_, Event}, {_, NestCount, [Last|Terms], ViewSt}) ->
    {collect_object, NestCount, [[Event] ++ Last] ++ Terms, ViewSt}.

send_row(Row, #viewst{owner=Owner, ref=Ref}=ViewSt) ->
    Owner ! {Ref, {row, Row}},
    maybe_continue_decoding(ViewSt).


%% eventually wait for the next call from the parent
maybe_continue_decoding(#viewst{parent=Parent,
                                owner=Owner,
                                ref=Ref,
                                client_ref=ClientRef,
                                async=once}=ViewSt) ->
    receive
        {Ref, stream_next} ->
            {wait_rows1, 0, [[]], ViewSt};
        {Ref, cancel} ->
            hackney:close(ClientRef),
            %% unregister the stream
            ets:delete(couchbeam_view_streams, Ref),
            %% tell the parent we exited
            Owner ! {Ref, ok},
            %% and exit
            exit(normal);
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {maybe_continue_decoding, ViewSt});
        Else ->
            error_logger:error_msg("Unexpected message: ~w~n", [Else]),
            %% unregister the stream
            ets:delete(couchbeam_view_streams, Ref),
            %% report the error
            report_error(Else, Ref, Owner),
            exit(Else)
    after 5000 ->
            erlang:hibernate(?MODULE, maybe_continue_decoding, [ViewSt])
    end;

maybe_continue_decoding(#viewst{parent=Parent,
                                owner=Owner,
                                ref=Ref,
                                client_ref=ClientRef}=ViewSt) ->
    receive
        {Ref, cancel} ->
            hackney:close(ClientRef),
            Owner ! {Ref, ok},
            exit(normal);
        {Ref, pause} ->
            erlang:hibernate(?MODULE, maybe_continue_decoding, [ViewSt]);
        {Ref, resume} ->
            {wait_rows1, 0, [[]], ViewSt};
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                  {maybe_continue_decoding, ViewSt});
        Else ->
            error_logger:error_msg("Unexpected message: ~w~n", [Else]),
            report_error(Else, Ref, Owner),
            exit(Else)
    after 0 ->
        {wait_rows1, 0, [[]], ViewSt}
    end.

report_error({error, _What}=Error, Ref, Pid) ->
    Pid ! {Ref, Error};
report_error(What, Ref, Pid) ->
    Pid ! {Ref, {error, What}}.
