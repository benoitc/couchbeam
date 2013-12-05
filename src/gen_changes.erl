%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

%% @doc gen_changes CouchDB continuous changes consumer behavior
%% This behaviour allws you to create easily a server that consume
%% Couchdb continuous changes

-module(gen_changes).

-include("couchbeam.hrl").

-behavior(gen_server).

-export([start_link/4]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([behaviour_info/1]).

-export([call/2,
         call/3,
         cast/2]).

-export([stop/1, get_seq/1]).


behaviour_info(callbacks) ->
    [{init, 1},
     {handle_change, 2},
     {handle_call, 3},
     {handle_cast, 2},
     {handle_info, 2},
     {terminate, 2}];
behaviour_info(_) ->
    undefined.

call(Name, Request) ->
    gen_server:call(Name, Request).

call(Name, Request, Timeout) ->
    gen_server:call(Name, Request, Timeout).

cast(Dest, Request) ->
    gen_server:cast(Dest, Request).

%% @doc create a gen_changes process as part of a supervision tree.
%% The function should be called, directly or indirectly, by the supervisor.
%% @spec start_link(Module, Db::db(), Options::changesoptions(),
%%                  InitArgs::list()) -> term()
%%       changesoptions() = [changeoption()]
%%       changeoption() = {include_docs, string()} |
%%                  {filter, string()} |
%%                  {since, integer()|string()} |
%%                  {heartbeat, string()|boolean()}
start_link(Module, Db, Options, InitArgs) ->
    gen_server:start_link(?MODULE, [Module, Db, Options, InitArgs], []).

init([Module, Db, Options, InitArgs]) ->
    case Module:init(InitArgs) of
        {ok, ModState} ->
            case couchbeam_changes:follow(Db, Options) of
            {ok, StreamRef} ->
                LastSeq = proplists:get_value(since, Options, 0),
                {ok, #gen_changes_state{stream_ref=StreamRef,
                                        mod=Module,
                                        modstate=ModState,
                                        db=Db,
                                        options=Options,
                                        last_seq=LastSeq}};
            {error, Error} ->
                Module:terminate(Error, ModState),
                {stop, Error}
            end;
        Error ->
            Error
    end.

stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop).

get_seq(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_seq).

handle_call(get_seq, _From, State=#gen_changes_state{last_seq=Seq}) ->
    {reply, Seq, State};
handle_call(Request, From,
            State=#gen_changes_state{mod=Module, modstate=ModState}) ->
    case Module:handle_call(Request, From, ModState) of
        {reply, Reply, NewModState} ->
            {reply, Reply, State#gen_changes_state{modstate=NewModState}};
        {reply, Reply, NewModState, A}
          when A =:= hibernate orelse is_number(A) ->
            {reply, Reply, State#gen_changes_state{modstate=NewModState}, A};
        {noreply, NewModState} ->
            {noreply, State#gen_changes_state{modstate=NewModState}};
        {noreply, NewModState, A} when A =:= hibernate orelse is_number(A) ->
            {noreply, State#gen_changes_state{modstate=NewModState}, A};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#gen_changes_state{modstate=NewModState}};
        {stop, Reason, Reply, NewModState} ->
            {stop, Reason, Reply, State#gen_changes_state{modstate=NewModState}}
  end.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State=#gen_changes_state{mod=Module, modstate=ModState}) ->
    case Module:handle_cast(Msg, ModState) of
        {noreply, NewModState} ->
            {noreply, State#gen_changes_state{modstate=NewModState}};
        {noreply, NewModState, A} when A =:= hibernate orelse is_number(A) ->
            {noreply, State#gen_changes_state{modstate=NewModState}, A};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#gen_changes_state{modstate=NewModState}}
    end.


handle_info({Ref, Msg},
        State=#gen_changes_state{mod=Module, modstate=ModState,
            stream_ref=Ref}) ->

    State2 = case Msg of
        {done, LastSeq} ->
            State#gen_changes_state{last_seq=LastSeq};
        {change, Change} ->
            Seq = couchbeam_doc:get_value(<<"seq">>, Change),
            State#gen_changes_state{last_seq=Seq}
    end,

    case catch Module:handle_change(Msg, ModState) of
        {noreply, NewModState} ->
            {noreply, State2#gen_changes_state{modstate=NewModState}};
        {noreply, NewModState, A} when A =:= hibernate orelse is_number(A) ->
            {noreply, State2#gen_changes_state{modstate=NewModState}, A};
        {stop, Reason, NewModState} ->
            {stop, Reason, State2#gen_changes_state{modstate=NewModState}}
    end;


handle_info({Ref, {error, Error}},
        State=#gen_changes_state{stream_ref=Ref, last_seq=LastSeq}) ->
    handle_info({error, [Error, {last_seq, LastSeq}]}, State);

handle_info(Info, State=#gen_changes_state{mod=Module, modstate=ModState}) ->
    case Module:handle_info(Info, ModState) of
        {noreply, NewModState} ->
            {noreply, State#gen_changes_state{modstate=NewModState}};
        {noreply, NewModState, A} when A =:= hibernate orelse is_number(A) ->
            {noreply, State#gen_changes_state{modstate=NewModState}, A};
        {stop, Reason, NewModState} ->
            {stop, Reason, State#gen_changes_state{modstate=NewModState}}
    end.

code_change(_OldVersion, State, _Extra) ->
    %% TODO:  support code changes?
    {ok, State}.

terminate(Reason, #gen_changes_state{stream_ref=Ref,
        mod=Module, modstate=ModState}) ->
    Module:terminate(Reason, ModState),
    couchbeam_changes:cancel_stream(Ref),
    ok.
