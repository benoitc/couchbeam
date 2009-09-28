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
-module(lhttpc_manager_tests).

-include_lib("eunit/include/eunit.hrl").

-define(HOST, "www.example.com").
-define(PORT, 666).
-define(SSL, false).

%%% Eunit setup stuff

start_app() ->
    ok = application:start(crypto),
    ok = application:start(ssl),
    ok = application:start(lhttpc).

stop_app(_) ->
    ok = application:stop(lhttpc),
    ok = application:stop(ssl),
    ok = application:stop(crypto).

manager_test_() ->
    {inorder,
        {setup, fun start_app/0, fun stop_app/1, [
                ?_test(empty_manager()),
                ?_test(one_socket()),
                ?_test(many_sockets()),
                ?_test(closed_race_cond())
            ]}
    }.

%%% Tests

empty_manager() ->
    ?assertEqual(no_socket, gen_server:call(lhttpc_manager,
            {socket, self(), ?HOST, ?PORT, ?SSL})).

one_socket() ->
    {LS, Socket} = socket_server:open(),
    gen_tcp:close(LS), % no use of this
    give_away(Socket, ?HOST, ?PORT, ?SSL),
    ?assertEqual({ok, Socket}, gen_server:call(lhttpc_manager,
            {socket, self(), ?HOST, ?PORT, ?SSL})),
    ?assertEqual(no_socket,  gen_server:call(lhttpc_manager,
            {socket, self(), ?HOST, ?PORT, ?SSL})),
    gen_tcp:close(Socket).

many_sockets() ->
    {LS, Socket1} = socket_server:open(),
    {ok, Port} = inet:port(LS),
    socket_server:accept(LS),
    socket_server:accept(LS),
    Socket2 = socket_server:connect(Port),
    Socket3 = socket_server:connect(Port),
    gen_tcp:close(LS),
    give_away(Socket1, ?HOST, ?PORT, ?SSL),
    give_away(Socket2, ?HOST, ?PORT, ?SSL),
    give_away(Socket3, ?HOST, ?PORT, ?SSL),
    ?assertEqual({ok, Socket3}, gen_server:call(lhttpc_manager,
            {socket, self(), ?HOST, ?PORT, ?SSL})),
    ?assertEqual({ok, Socket2}, gen_server:call(lhttpc_manager,
            {socket, self(), ?HOST, ?PORT, ?SSL})),
    ?assertEqual({ok, Socket1}, gen_server:call(lhttpc_manager,
            {socket, self(), ?HOST, ?PORT, ?SSL})),
    link(whereis(lhttpc_manager)), % want to make sure it doesn't crash
    lhttpc_manager ! {tcp_closed, Socket1},
    ?assertEqual(0, lhttpc_manager:connection_count()),
    unlink(whereis(lhttpc_manager)),
    gen_tcp:close(Socket1),
    gen_tcp:close(Socket2),
    gen_tcp:close(Socket3).

closed_race_cond() ->
    {LS, Socket} = socket_server:open(),
    gen_tcp:close(LS), % no use of this
    give_away(Socket, ?HOST, ?PORT, ?SSL),
    Pid = self(),
    ManagerPid = whereis(lhttpc_manager),
    true = erlang:suspend_process(ManagerPid),
    spawn_link(fun() -> 
                Pid ! {result, gen_server:call(lhttpc_manager,
                        {socket, self(), ?HOST, ?PORT, ?SSL})}
        end),
    erlang:yield(), % make sure that the spawned process has run
    gen_tcp:close(Socket), % a closed message should be sent to the manager
    true = erlang:resume_process(ManagerPid),
    Result = receive {result, R} -> R end,
    ?assertEqual(no_socket, Result).

%%% Helpers functions

give_away(Socket, Host, Port, Ssl) ->
    gen_tcp:controlling_process(Socket, whereis(lhttpc_manager)),
    gen_server:cast(lhttpc_manager, {done, Host, Port, Ssl, Socket}).
