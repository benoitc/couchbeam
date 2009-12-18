%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of Erlang Training and Consulting Ltd. nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY Erlang Training and Consulting Ltd. ''AS IS''
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Erlang Training and Consulting Ltd. BE
%%% LIABLE SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% ----------------------------------------------------------------------------

%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%% @doc Connection manager for the HTTP client.
%%% This gen_server is responsible for keeping track of persistent
%%% connections to HTTP servers. The only interesting API is
%%% `connection_count/0' and `connection_count/1'.
%%% The gen_server is supposed to be started by a supervisor, which is
%%% normally {@link lhttpc_sup}.
%%% @end
-module(lhttpc_manager).

-export([
        start_link/0,
        connection_count/0,
        connection_count/1,
        update_connection_timeout/1
    ]).
-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2
    ]).

-behaviour(gen_server).

-record(httpc_man, {
        destinations = dict:new(),
        sockets = dict:new(),
        timeout = 300000 :: non_neg_integer()
    }).

%% @spec () -> Count
%%    Count = integer()
%% @doc Returns the total number of active connections maintained by the
%% httpc manager.
%% @end
-spec connection_count() -> non_neg_integer().
connection_count() ->
    gen_server:call(?MODULE, connection_count).

%% @spec (Destination) -> Count
%%    Destination = {Host, Port, Ssl}
%%    Host = string()
%%    Port = integer()
%%    Ssl = bool()
%%    Count = integer()
%% @doc Returns the number of active connections to the specific
%% `Destination' maintained by the httpc manager.
%% @end
-spec connection_count({string(), pos_integer(), bool()}) ->
    non_neg_integer().
connection_count({Host, Port, Ssl}) ->
    Destination = {string:to_lower(Host), Port, Ssl},
    gen_server:call(?MODULE, {connection_count, Destination}).

%% @spec (Timeout) -> ok
%%    Timeout = intetger()
%% @doc Updates the timeout for persistent connections.
%% This will only affect future sockets handed to the manager. The sockets
%% already managed will keep their timers.
%% @end
-spec update_connection_timeout(non_neg_integer()) -> ok.
update_connection_timeout(Milliseconds) ->
    gen_server:cast(?MODULE, {update_timeout, Milliseconds}).

%% @spec () -> {ok, pid()}
%% @doc Starts and link to the gen server.
%% This is normally called by a supervisor.
%% @end
-spec start_link() -> {ok, pid()} | {error, allready_started}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nil, []).

%% @hidden
-spec init(any()) -> {ok, #httpc_man{}}.
init(_) ->
    process_flag(priority, high),
    {ok, Timeout} = application:get_env(lhttpc, connection_timeout),
    {ok, #httpc_man{timeout = Timeout}}.

%% @hidden
-spec handle_call(any(), any(), #httpc_man{}) ->
    {reply, any(), #httpc_man{}}.
handle_call({socket, Pid, Host, Port, Ssl}, _, State) ->
    {Reply, NewState} = find_socket({Host, Port, Ssl}, Pid, State),
    {reply, Reply, NewState};
handle_call(connection_count, _, State) ->
    {reply, dict:size(State#httpc_man.sockets), State};
handle_call({connection_count, Destination}, _, State) ->
    Count = case dict:find(Destination, State#httpc_man.destinations) of
        {ok, Sockets} -> length(Sockets);
        error         -> 0
    end,
    {reply, Count, State};
handle_call(_, _, State) ->
    {reply, {error, unknown_request}, State}.

%% @hidden
-spec handle_cast(any(), #httpc_man{}) -> {noreply, #httpc_man{}}.
handle_cast({done, Host, Port, Ssl, Socket}, State) ->
    NewState = store_socket({Host, Port, Ssl}, Socket, State),
    {noreply, NewState};
handle_cast({update_timeout, Milliseconds}, State) ->
    {noreply, State#httpc_man{timeout = Milliseconds}};
handle_cast(_, State) ->
    {noreply, State}.

%% @hidden
-spec handle_info(any(), #httpc_man{}) -> {noreply, #httpc_man{}}.
handle_info({tcp_closed, Socket}, State) ->
    {noreply, remove_socket(Socket, State)};
handle_info({ssl_closed, Socket}, State) ->
    {noreply, remove_socket(Socket, State)};
handle_info({timeout, Socket}, State) ->
    {noreply, remove_socket(Socket, State)};
handle_info({tcp_error, Socket, _}, State) ->
    {noreply, remove_socket(Socket, State)};
handle_info({ssl_error, Socket, _}, State) ->
    {noreply, remove_socket(Socket, State)};
handle_info({tcp, Socket, _}, State) ->
    {noreply, remove_socket(Socket, State)}; % got garbage
handle_info({ssl, Socket, _}, State) ->
    {noreply, remove_socket(Socket, State)}; % got garbage
handle_info(_, State) ->
    {noreply, State}.

%% @hidden
-spec terminate(any(), #httpc_man{}) -> ok.
terminate(_, State) ->
    close_sockets(State#httpc_man.sockets).

%% @hidden
-spec code_change(any(), #httpc_man{}, any()) -> #httpc_man{}.
code_change(_, State, _) ->
    State.

find_socket({_, _, Ssl} = Dest, Pid, State) ->
    Dests = State#httpc_man.destinations,
    case dict:find(Dest, Dests) of
        {ok, [Socket | Sockets]} ->
            lhttpc_sock:setopts(Socket, [{active, false}], Ssl),
            case lhttpc_sock:controlling_process(Socket, Pid, Ssl) of
                ok ->
                    {_, Timer} = dict:fetch(Socket, State#httpc_man.sockets),
                    cancel_timer(Timer, Sockets),
                    NewState = State#httpc_man{
                        destinations = update_dest(Dest, Sockets, Dests),
                        sockets = dict:erase(Socket, State#httpc_man.sockets)
                    },
                    {{ok, Socket}, NewState};
                {error, badarg} -> % Pid has timed out, reuse for someone else
                    lhttpc_sock:setopts(Socket, [{active, once}], Ssl),
                    {no_socket, State};
                _ -> % something wrong with the socket; remove it, try again
                    find_socket(Dest, Pid, remove_socket(Socket, State))
            end;
        error ->
            {no_socket, State}
    end.

remove_socket(Socket, State) ->
    Dests = State#httpc_man.destinations,
    case dict:find(Socket, State#httpc_man.sockets) of
        {ok, {{_, _, Ssl} = Dest, Timer}} ->
            cancel_timer(Timer, Socket),
            lhttpc_sock:close(Socket, Ssl),
            Sockets = lists:delete(Socket, dict:fetch(Dest, Dests)),
            State#httpc_man{
                destinations = update_dest(Dest, Sockets, Dests),
                sockets = dict:erase(Socket, State#httpc_man.sockets)
            };
        error ->
            State
    end.

store_socket({_, _, Ssl} = Dest, Socket, State) ->
    Timeout = State#httpc_man.timeout,
    Timer = erlang:send_after(Timeout, self(), {timeout, Socket}),
    % the socket might be closed from the other side
    lhttpc_sock:setopts(Socket, [{active, once}], Ssl),
    Dests = State#httpc_man.destinations,
    Sockets = case dict:find(Dest, Dests) of
        {ok, S} -> [Socket | S];
        error   -> [Socket]
    end,
    State#httpc_man{
        destinations = dict:store(Dest, Sockets, Dests),
        sockets = dict:store(Socket, {Dest, Timer}, State#httpc_man.sockets)
    }.

update_dest(Destination, [], Destinations) ->
    dict:erase(Destination, Destinations);
update_dest(Destination, Sockets, Destinations) ->
    dict:store(Destination, Sockets, Destinations).

close_sockets(Sockets) ->
    lists:foreach(fun({Socket, {{_, _, Ssl}, Timer}}) ->
                lhttpc_sock:close(Socket, Ssl),
                erlang:cancel_timer(Timer)
        end, dict:to_list(Sockets)).

cancel_timer(Timer, Socket) ->
    case erlang:cancel_timer(Timer) of
        false ->
            receive
                {timeout, Socket} -> ok
            after 
                0 -> ok
            end;
        _     -> ok
    end.
