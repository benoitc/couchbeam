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
-module(socket_server).

-export([open/0, connect/1, listen/0, accept/1]).
-export([do_accept/2]).

open() ->
    {LS, Port} = listen(),
    accept(LS),
    {ok, Port} = inet:port(LS),
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [{active, false}]),
    {LS, Socket}.

connect(Port) ->
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [{active, false}]),
    Socket.

listen() ->
    {ok, LS} = gen_tcp:listen(0, [{active, false}, {ip, {127,0,0,1}}]),
    {ok, Port} = inet:port(LS),
    {LS, Port}.

accept(LS) ->
    Pid = spawn_link(?MODULE, do_accept, [LS, self()]),
    receive in_accept -> ok end,
    Pid.

do_accept(LS, Parent) ->
    erlang:send_after(50, Parent, in_accept),
    {ok, S} = gen_tcp:accept(LS),
    {error, closed} = gen_tcp:recv(S, 0).
