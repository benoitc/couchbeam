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




-include("couchbeam.hrl").

-record(state, {parent,
                owner,
                req,
                ref,
                mref,
                client_ref=nil,
                buffer = <<>>,
                rows = [],
                index = 0,
                async=normal}).

-record(viewst, {parent,
                 owner,
                 ref,
                 mref,
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

    %% monitor the process receiving the messages
    MRef = erlang:monitor(process, Owner),

    %% tell to the parent that we are ok
    proc_lib:init_ack(Parent, {ok, self()}),

    InitState = #state{parent=Parent,
                       owner=Owner,
                       req=Req,
                       ref=StreamRef,
                       mref=MRef,
                       async=Async},

    %% connect to the view
    try
        case do_init_stream(Req, InitState) of
            {ok, State} ->
                %% register the stream
                ets:insert(couchbeam_view_streams, [{StreamRef, self()}]),
                %% start the loop
                loop(State);
            Error ->
                report_error(Error, StreamRef, Owner)
        end
    after
        %% Always clean up the monitor reference
        erlang:demonitor(MRef, [flush])
    end,
    ok.

do_init_stream({#db{options=Opts}, Url, Args}, #state{mref=MRef}=State) ->
    %% we are doing the request asynchronously
    FinalOpts = [{async, once} | Opts],
    Reply = case Args#view_query_args.method of
        get ->
            couchbeam_httpc:request(get, Url, [], <<>>, FinalOpts);
        post ->
            Body = couchbeam_ejson:encode(#{<<"keys">> => Args#view_query_args.keys}),
            Headers = [{<<"Content-Type">>, <<"application/json">>}],
            couchbeam_httpc:request(post, Url, Headers, Body, FinalOpts)
    end,

    case Reply of
        {ok, Ref} ->
            Req = hackney:request_info(Ref),
            Mod = proplists:get_value(transport, Req),
            S = proplists:get_value(socket, Req),
            _  = Mod:controlling_process(S, self()),
            receive
                {'DOWN', MRef, _, _, _} ->
                    %% parent exited there is no need to continue
                    exit(normal);
                {hackney_response, Ref, {status, 200, _}} ->
                    {ok, State#state{client_ref=Ref}};

                {hackney_response, Ref, {status, 404, _}} ->
                    {error, not_found};
                {hackney_response, Ref, {status, Status, Reason}} ->
                    {error, {http_error, Status, Reason}};
                {hackney_response, Ref, {error, Reason}} ->
                    {error, Reason}
            after ?TIMEOUT ->
                    {error, timeout}
            end;
        Error ->
            {error, Error}
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
            %% finalize: decode accumulated buffer and stream rows
            finalize_and_stream(State);
        {hackney_response, ClientRef, Data} when is_binary(Data) ->
            loop(State#state{buffer = <<(State#state.buffer)/binary, Data/binary>>});
        {hackney_response, ClientRef, Error} ->
            ets:delete(couchbeam_view_streams, StreamRef),
            %% report the error
            report_error(Error, StreamRef, Owner),
            exit(Error)
    end.

finalize_and_stream(#state{owner=Owner,
                           ref=StreamRef,
                           client_ref=ClientRef,
                           buffer=Buffer,
                           async=Async}=State) ->
    %% stop the request and release the socket
    catch hackney:stop_async(ClientRef),
    catch hackney:skip_body(ClientRef),
    %% decode the JSON and stream rows
    try
        Map = couchbeam_ejson:decode(Buffer),
        Rows = maps:get(<<"rows">>, Map, []),
        case Async of
            once ->
                stream_once_loop(State#state{rows=Rows, index=0});
            _ ->
                lists:foreach(fun(Row) -> Owner ! {StreamRef, {row, Row}} end, Rows),
                ets:delete(couchbeam_view_streams, StreamRef),
                Owner ! {StreamRef, done}
        end
    catch
        Class:Reason ->
            ets:delete(couchbeam_view_streams, StreamRef),
            Owner ! {StreamRef, {error, {Class, Reason}}}
    end.

stream_once_loop(#state{rows=Rows, index=Idx, owner=Owner, ref=Ref,
                        mref=MRef, client_ref=ClientRef, parent=Parent}=State) ->
    case lists:nthtail(Idx, Rows) of
        [] ->
            ets:delete(couchbeam_view_streams, Ref),
            Owner ! {Ref, done};
        [Row | _] ->
            Owner ! {Ref, {row, Row}},
            receive
                {'DOWN', MRef, _, _, _} ->
                    exit(normal);
                {Ref, stream_next} ->
                    stream_once_loop(State#state{index=Idx+1});
                {Ref, cancel} ->
                    hackney:close(ClientRef),
                    ets:delete(couchbeam_view_streams, Ref),
                    Owner ! {Ref, ok},
                    exit(normal);
                {Ref, pause} ->
                    erlang:hibernate(?MODULE, stream_once_loop, [State]);
                {Ref, resume} ->
                    stream_once_loop(State);
                {system, From, Request} ->
                    sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
                                          {loop, State});
                Else ->
                    error_logger:error_msg("Unexpected message: ~w~n", [Else]),
                    ets:delete(couchbeam_view_streams, Ref),
                    report_error(Else, Ref, Owner),
                    exit(Else)
            end
    end.

maybe_continue(#state{parent=Parent, owner=Owner, ref=Ref, mref=MRef,
                      async=once}=State) ->

    receive
        {'DOWN', MRef, _, _, _} ->
            %% parent exited there is no need to continue
            maybe_close(State),
            exit(normal);
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
            maybe_close(State),
            exit(normal);
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
system_terminate(Reason, _, _, #state{ref=StreamRef,
                                      client_ref=ClientRef}) ->
    hackney:close(ClientRef),
    %% unregister the stream
    catch ets:delete(couchbeam_view_streams, StreamRef),
    exit(Reason).

system_code_change(Misc, _, _, _) ->
    {ok, Misc}.


%%% json streaming via once loop (no external decoder)

report_error({error, _What}=Error, Ref, Pid) ->
    Pid ! {Ref, Error};
report_error(What, Ref, Pid) ->
    Pid ! {Ref, {error, What}}.
