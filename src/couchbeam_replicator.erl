%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.
%%%
%%% A client-side CouchDB *pull* replicator: it reads the changes feed and
%%% document revisions from a source CouchDB and hands them to a pluggable
%%% local target (a module implementing the `couchbeam_replicator' behaviour,
%%% e.g. `couchbeam_replicator_ets'). Document revision history is preserved.
%%% Replication is resumable: the target persists the last processed source
%%% sequence under a stable replication id.
%%%
%%% Pull only (CouchDB -> app). Each target entry is `{Doc, Atts}' where Atts
%%% maps attachment name to body bytes as delivered by CouchDB (the doc's
%%% `_attachments' metadata records content type and any encoding, e.g. gzip).

-module(couchbeam_replicator).
-behaviour(gen_statem).

%% public API
-export([start_link/4,
         stop/1,
         status/1,
         pause/1,
         resume/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([starting/3, replicating/3, paused/3]).

-include("couchbeam.hrl").

-type repl_opt() :: {mode, oneshot | continuous}
                  | {batch_size, pos_integer()}
                  | {interval, pos_integer()}
                  | {repl_id, binary()}
                  | {notify, pid()}
                  | {changes_options, list()}.
-export_type([repl_opt/0]).

%% Target behaviour: the local store the source is replicated into.
-callback init(Args :: term()) ->
    {ok, TState :: term()} | {error, term()}.
-callback revs_diff(IdRevs :: [{DocId :: binary(), [Rev :: binary()]}], TState :: term()) ->
    {ok, Missing :: [{DocId :: binary(), [Rev :: binary()]}], TState :: term()}.
-callback write_docs(Entries :: [{Doc :: map(), Atts :: #{binary() => binary()}}], TState :: term()) ->
    {ok, TState :: term()} | {error, term()}.
-callback read_checkpoint(ReplId :: binary(), TState :: term()) ->
    {ok, Seq :: term() | nil, TState :: term()}.
-callback write_checkpoint(ReplId :: binary(), Seq :: term(), TState :: term()) ->
    {ok, TState :: term()}.
-callback terminate(Reason :: term(), TState :: term()) -> any().
-optional_callbacks([terminate/2]).

-record(data, {
    source                      :: db(),
    target_mod                  :: module(),
    target_state                :: term(),
    repl_id                     :: binary(),
    mode = oneshot              :: oneshot | continuous,
    batch_size = 100            :: pos_integer(),
    interval = 5000             :: pos_integer(),
    changes_options = []        :: list(),
    notify                      :: pid() | undefined,
    since = 0                   :: term(),
    docs_written = 0            :: non_neg_integer()
}).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start a pull replication from Source into TargetMod.
-spec start_link(Source :: db(), TargetMod :: module(),
                 TargetArgs :: term(), Opts :: [repl_opt()]) ->
    {ok, pid()} | {error, term()}.
start_link(Source, TargetMod, TargetArgs, Opts) ->
    gen_statem:start_link(?MODULE, {Source, TargetMod, TargetArgs, Opts}, []).

%% @doc Stop the replicator.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

%% @doc Current replication status.
-spec status(pid()) ->
    {ok, #{state := atom(), repl_id := binary(),
           since := term(), docs_written := non_neg_integer()}}.
status(Pid) ->
    gen_statem:call(Pid, status).

%% @doc Pause a running replication.
-spec pause(pid()) -> ok.
pause(Pid) ->
    gen_statem:call(Pid, pause).

%% @doc Resume a paused replication.
-spec resume(pid()) -> ok.
resume(Pid) ->
    gen_statem:call(Pid, resume).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

callback_mode() ->
    state_functions.

init({Source, TargetMod, TargetArgs, Opts}) ->
    ReplId = case proplists:get_value(repl_id, Opts) of
                 undefined -> default_repl_id(Source, TargetMod);
                 Id when is_binary(Id) -> Id
             end,
    case TargetMod:init(TargetArgs) of
        {ok, TState} ->
            Data = #data{source = Source,
                         target_mod = TargetMod,
                         target_state = TState,
                         repl_id = ReplId,
                         mode = proplists:get_value(mode, Opts, oneshot),
                         batch_size = proplists:get_value(batch_size, Opts, 100),
                         interval = proplists:get_value(interval, Opts, 5000),
                         changes_options = proplists:get_value(changes_options, Opts, []),
                         notify = proplists:get_value(notify, Opts)},
            {ok, starting, Data, {next_event, internal, init}};
        {error, Reason} ->
            {stop, Reason}
    end.

terminate(Reason, _State, #data{target_mod = TMod, target_state = TS}) ->
    case erlang:function_exported(TMod, terminate, 2) of
        true -> try TMod:terminate(Reason, TS) catch _:_ -> ok end;
        false -> ok
    end,
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%====================================================================
%% States
%%====================================================================

%% starting: read the source info and the target checkpoint, then replicate.
starting(internal, init, Data) ->
    #data{source = Source, target_mod = TMod, target_state = TS,
          repl_id = ReplId} = Data,
    case couchbeam:db_info(Source) of
        {ok, _Info} ->
            {ok, Checkpoint, TS1} = TMod:read_checkpoint(ReplId, TS),
            Since = case Checkpoint of nil -> 0; S -> S end,
            Data1 = Data#data{target_state = TS1, since = Since},
            {next_state, replicating, Data1, {next_event, internal, pull}};
        {error, Reason} ->
            notify(Data, {error, Reason}),
            {stop, {source_unavailable, Reason}}
    end;
starting(EventType, EventContent, Data) ->
    handle_common(starting, EventType, EventContent, Data).

%% replicating: pull one batch at a time until caught up.
replicating(internal, pull, Data) ->
    case pull_batch(Data) of
        {caught_up, Data1} ->
            case Data1#data.mode of
                oneshot ->
                    notify(Data1, done),
                    {stop, normal, Data1};
                continuous ->
                    {keep_state, Data1, {state_timeout, Data1#data.interval, pull}}
            end;
        {more, Data1} ->
            {keep_state, Data1, {next_event, internal, pull}};
        {error, Reason, Data1} ->
            notify(Data1, {error, Reason}),
            {stop, {replication_error, Reason}, Data1}
    end;
replicating(state_timeout, pull, _Data) ->
    {keep_state_and_data, {next_event, internal, pull}};
replicating({call, From}, pause, Data) ->
    {next_state, paused, Data, {reply, From, ok}};
replicating(EventType, EventContent, Data) ->
    handle_common(replicating, EventType, EventContent, Data).

%% paused: do nothing until resumed.
paused({call, From}, resume, Data) ->
    {next_state, replicating, Data,
     [{reply, From, ok}, {next_event, internal, pull}]};
paused(EventType, EventContent, Data) ->
    handle_common(paused, EventType, EventContent, Data).

%%====================================================================
%% Common event handling
%%====================================================================

handle_common(State, {call, From}, status, Data) ->
    {keep_state_and_data, {reply, From, {ok, status_map(State, Data)}}};
handle_common(_State, {call, From}, stop, Data) ->
    {stop_and_reply, normal, {reply, From, ok}, Data};
handle_common(_State, {call, From}, _Req, _Data) ->
    {keep_state_and_data, {reply, From, {error, unsupported}}};
handle_common(_State, _Type, _Content, _Data) ->
    keep_state_and_data.

%%====================================================================
%% Replication batch
%%====================================================================

pull_batch(Data) ->
    #data{source = Source, since = Since, batch_size = N,
          changes_options = Extra, target_mod = TMod,
          target_state = TS, repl_id = ReplId} = Data,
    Opts = [{since, Since}, {style, all_docs}, {limit, N} | Extra],
    case couchbeam_changes:follow_once(Source, Opts) of
        {ok, LastSeq, []} ->
            {ok, TS1} = TMod:write_checkpoint(ReplId, LastSeq, TS),
            notify(Data, {checkpoint, LastSeq}),
            {caught_up, Data#data{since = LastSeq, target_state = TS1}};
        {ok, LastSeq, Changes} ->
            IdRevs = changes_to_idrevs(Changes),
            {ok, Missing, TS1} = TMod:revs_diff(IdRevs, TS),
            case fetch_missing(Source, Missing) of
                {ok, Entries} ->
                    case TMod:write_docs(Entries, TS1) of
                        {ok, TS2} ->
                            {ok, TS3} = TMod:write_checkpoint(ReplId, LastSeq, TS2),
                            Written = Data#data.docs_written + length(Entries),
                            notify(Data, {docs_written, length(Entries)}),
                            notify(Data, {checkpoint, LastSeq}),
                            {more, Data#data{since = LastSeq, target_state = TS3,
                                             docs_written = Written}};
                        {error, Reason} ->
                            {error, Reason, Data#data{target_state = TS1}}
                    end;
                {error, Reason} ->
                    {error, Reason, Data#data{target_state = TS1}}
            end;
        {error, Reason} ->
            {error, Reason, Data}
    end.

changes_to_idrevs(Changes) ->
    [{maps:get(<<"id">>, C),
      [maps:get(<<"rev">>, R) || R <- maps:get(<<"changes">>, C, [])]}
     || C <- Changes].

%% Fetch each missing revision with its revision history and attachment bodies.
fetch_missing(Source, Missing) ->
    try
        Entries = lists:flatmap(
            fun({DocId, Revs}) ->
                [fetch_rev(Source, DocId, Rev) || Rev <- Revs]
            end, Missing),
        {ok, Entries}
    catch
        throw:{fetch_error, Reason} -> {error, Reason}
    end.

%% A revision with attachments comes back as a multipart stream; one without
%% attachments (or a deletion) comes back as a plain JSON document.
fetch_rev(Source, DocId, Rev) ->
    case couchbeam:open_doc(Source, DocId,
                            [{rev, Rev}, {revs, true}, {"attachments", true}]) of
        {ok, {multipart, State}} ->
            collect_multipart(State, undefined, <<>>, #{});
        {ok, Doc} when is_map(Doc) ->
            {Doc, #{}};
        {error, Reason} ->
            throw({fetch_error, {DocId, Rev, Reason}})
    end.

%% Drive couchbeam:stream_doc/1 to reassemble the document and its attachment
%% bodies (kept as delivered; encoding is recorded in the doc's _attachments).
collect_multipart(State, Doc, Buf, Atts) ->
    case couchbeam:stream_doc(State) of
        {doc, NewDoc, NState} ->
            collect_multipart(NState, NewDoc, Buf, Atts);
        {att, _Name, NState} ->
            collect_multipart(NState, Doc, <<>>, Atts);
        {att_body, _Name, Chunk, NState} ->
            collect_multipart(NState, Doc, <<Buf/binary, Chunk/binary>>, Atts);
        {att_eof, Name, NState} ->
            collect_multipart(NState, Doc, <<>>, Atts#{Name => Buf});
        eof ->
            {Doc, Atts};
        {error, Reason} ->
            throw({fetch_error, {multipart, Reason}})
    end.

%%====================================================================
%% Helpers
%%====================================================================

default_repl_id(#db{server = Server} = Source, TargetMod) ->
    ServerUrl = couchbeam_httpc:server_url(Server),
    DbUrl = couchbeam_httpc:db_url(Source),
    Base = iolist_to_binary([ServerUrl, "/", DbUrl, "|",
                             atom_to_binary(TargetMod, utf8)]),
    binary:encode_hex(crypto:hash(sha256, Base), lowercase).

status_map(State, #data{repl_id = ReplId, since = Since, docs_written = N}) ->
    #{state => State, repl_id => ReplId, since => Since, docs_written => N}.

notify(#data{notify = undefined}, _Msg) ->
    ok;
notify(#data{notify = Pid}, Msg) ->
    Pid ! {self(), Msg},
    ok.
