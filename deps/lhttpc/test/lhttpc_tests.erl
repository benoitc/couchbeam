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
-module(lhttpc_tests).

-import(webserver, [start/2]).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_STRING, "Great success!").

%%% Eunit setup stuff

start_app() ->
    ok = application:start(crypto),
    ok = application:start(ssl),
    ok = application:start(lhttpc).

stop_app(_) ->
    ok = application:stop(lhttpc),
    ok = application:stop(ssl),
    ok = application:stop(crypto).

tcp_test_() ->
    {inorder, 
        {setup, fun start_app/0, fun stop_app/1, [
                ?_test(simple_get()),
                ?_test(empty_get()),
                ?_test(get_with_mandatory_hdrs()),
                ?_test(no_content_length()),
                ?_test(no_content_length_1_0()),
                ?_test(simple_head()),
                ?_test(simple_head_atom()),
                ?_test(delete_no_content()),
                ?_test(delete_content()),
                ?_test(options_content()),
                ?_test(options_no_content()),
                ?_test(server_connection_close()),
                ?_test(client_connection_close()),
                ?_test(pre_1_1_server_connection()),
                ?_test(pre_1_1_server_keep_alive()),
                ?_test(simple_put()),
                ?_test(post()),
                ?_test(bad_url()),
                ?_test(persistent_connection()),
                ?_test(request_timeout()),
                ?_test(connection_timeout()),
                ?_test(suspended_manager()),
                ?_test(chunked_encoding()),
                ?_test(close_connection()),
                ?_test(message_queue()),
                ?_test(connection_count()) % just check that it's 0 (last)
            ]}
    }.

ssl_test_() ->
    {inorder,
        {setup, fun start_app/0, fun stop_app/1, [
                ?_test(ssl_get()),
                ?_test(ssl_post()),
                ?_test(ssl_chunked()),
                ?_test(connection_count()) % just check that it's 0 (last)
            ]}
    }.

other_test_() ->
    [
        ?_test(invalid_options())
    ].

%%% Tests

message_queue() ->
    receive X -> erlang:error({unexpected_message, X}) after 0 -> ok end.

simple_get() ->
    simple(get),
    simple("GET").

empty_get() ->
    Port = start(gen_tcp, [fun empty_body/5]),
    URL = url(Port, "/empty"),
    {ok, Response} = lhttpc:request(URL, "GET", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

get_with_mandatory_hdrs() ->
    Port = start(gen_tcp, [fun simple_response/5]),
    URL = url(Port, "/host"),
    Body = <<?DEFAULT_STRING>>,
    Hdrs = [
        {"content-length", integer_to_list(size(Body))},
        {"host", "localhost"}
    ],
    {ok, Response} = lhttpc:request(URL, "POST", Hdrs, Body, 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

no_content_length() ->
    Port = start(gen_tcp, [fun no_content_length/5]),
    URL = url(Port, "/no_cl"),
    {ok, Response} = lhttpc:request(URL, "GET", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

no_content_length_1_0() ->
    Port = start(gen_tcp, [fun no_content_length_1_0/5]),
    URL = url(Port, "/no_cl"),
    {ok, Response} = lhttpc:request(URL, "GET", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

simple_head() ->
    Port = start(gen_tcp, [fun head_response/5]),
    URL = url(Port, "/HEAD"),
    {ok, Response} = lhttpc:request(URL, "HEAD", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

simple_head_atom() ->
    Port = start(gen_tcp, [fun head_response/5]),
    URL = url(Port, "/head"),
    {ok, Response} = lhttpc:request(URL, head, [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

delete_no_content() ->
    Port = start(gen_tcp, [fun no_content_response/5]),
    URL = url(Port, "/delete_no_content"),
    {ok, Response} = lhttpc:request(URL, delete, [], 1000),
    ?assertEqual({204, "OK"}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

delete_content() ->
    Port = start(gen_tcp, [fun simple_response/5]),
    URL = url(Port, "/delete_content"),
    {ok, Response} = lhttpc:request(URL, "DELETE", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

options_no_content() ->
    Port = start(gen_tcp, [fun head_response/5]),
    URL = url(Port, "/options_no_content"),
    {ok, Response} = lhttpc:request(URL, "OPTIONS", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual(<<>>, body(Response)).

options_content() ->
    Port = start(gen_tcp, [fun simple_response/5]),
    URL = url(Port, "/options_content"),
    {ok, Response} = lhttpc:request(URL, "OPTIONS", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

server_connection_close() ->
    Port = start(gen_tcp, [fun respond_and_close/5]),
    URL = url(Port, "/close"),
    Body = pid_to_list(self()),
    {ok, Response} = lhttpc:request(URL, "PUT", [], Body, 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)),
    receive closed -> ok end.

client_connection_close() ->
    Port = start(gen_tcp, [fun respond_and_wait/5]),
    URL = url(Port, "/close"),
    Body = pid_to_list(self()),
    Hdrs = [{"Connection", "close"}],
    {ok, _} = lhttpc:request(URL, put, Hdrs, Body, 1000),
    % Wait for the server to see that socket has been closed
    receive closed -> ok end.

pre_1_1_server_connection() ->
    Port = start(gen_tcp, [fun pre_1_1_server/5]),
    URL = url(Port, "/close"),
    Body = pid_to_list(self()),
    {ok, _} = lhttpc:request(URL, put, [], Body, 1000),
    % Wait for the server to see that socket has been closed.
    % The socket should be closed by us since the server responded with a
    % 1.0 version, and not the Connection: keep-alive header.
    receive closed -> ok end.

pre_1_1_server_keep_alive() ->
    Port = start(gen_tcp, [fun simple_response/5, fun simple_response/5]),
    URL = url(Port, "/close"),
    {ok, Response1} = lhttpc:request(URL, get, [], [], 1000),
    {ok, Response2} = lhttpc:request(URL, get, [], [], 1000),
    ?assertEqual({200, "OK"}, status(Response1)),
    ?assertEqual({200, "OK"}, status(Response2)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response1)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response2)).

simple_put() ->
    simple(put),
    simple("PUT").

post() ->
    Port = start(gen_tcp, [fun copy_body/5]),
    URL = url(Port, "/post"),
    {X, Y, Z} = now(),
    Body = [
        "This is a rather simple post :)",
        integer_to_list(X),
        integer_to_list(Y),
        integer_to_list(Z)
    ],
    {ok, Response} = lhttpc:request(URL, "POST", [], Body, 1000),
    {StatusCode, ReasonPhrase} = status(Response),
    ?assertEqual(200, StatusCode),
    ?assertEqual("OK", ReasonPhrase),
    ?assertEqual(iolist_to_binary(Body), body(Response)).

bad_url() ->
    ?assertError(_, lhttpc:request(ost, "GET", [], 100)).

persistent_connection() ->
    Port = start(gen_tcp, [
            fun simple_response/5,
            fun simple_response/5,
            fun copy_body/5
        ]),
    URL = url(Port, "/persistent"),
    {ok, FirstResponse} = lhttpc:request(URL, "GET", [], 1000),
    Headers = [{"KeepAlive", "300"}], % shouldn't be needed
    {ok, SecondResponse} = lhttpc:request(URL, "GET", Headers, 1000),
    {ok, ThirdResponse} = lhttpc:request(URL, "POST", [], 1000),
    ?assertEqual({200, "OK"}, status(FirstResponse)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(FirstResponse)),
    ?assertEqual({200, "OK"}, status(SecondResponse)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(SecondResponse)),
    ?assertEqual({200, "OK"}, status(ThirdResponse)),
    ?assertEqual(<<>>, body(ThirdResponse)).

request_timeout() ->
    Port = start(gen_tcp, [fun very_slow_response/5]),
    URL = url(Port, "/slow"),
    ?assertEqual({error, timeout}, lhttpc:request(URL, get, [], 50)).

connection_timeout() ->
    Port = start(gen_tcp, [fun simple_response/5, fun simple_response/5]),
    URL = url(Port, "/close_conn"),
    lhttpc_manager:update_connection_timeout(50), % very short keep alive
    {ok, Response} = lhttpc:request(URL, get, [], 100),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)),
    timer:sleep(100),
    ?assertEqual(0,
        lhttpc_manager:connection_count({"localhost", Port, false})),
    lhttpc_manager:update_connection_timeout(300000). % set back

suspended_manager() ->
    Port = start(gen_tcp, [fun simple_response/5, fun simple_response/5]),
    URL = url(Port, "/persistent"),
    {ok, FirstResponse} = lhttpc:request(URL, get, [], 50),
    ?assertEqual({200, "OK"}, status(FirstResponse)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(FirstResponse)),
    Pid = whereis(lhttpc_manager),
    true = erlang:suspend_process(Pid),
    ?assertEqual({error, timeout}, lhttpc:request(URL, get, [], 50)),
    true = erlang:resume_process(Pid),
    ?assertEqual(1,
        lhttpc_manager:connection_count({"localhost", Port, false})),
    {ok, SecondResponse} = lhttpc:request(URL, get, [], 50),
    ?assertEqual({200, "OK"}, status(SecondResponse)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(SecondResponse)).

chunked_encoding() ->
    Port = start(gen_tcp, [fun chunked_response/5, fun chunked_response_t/5]),
    URL = url(Port, "/chunked"),
    {ok, FirstResponse} = lhttpc:request(URL, get, [], 50),
    ?assertEqual({200, "OK"}, status(FirstResponse)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(FirstResponse)),
    ?assertEqual("chunked", lhttpc_lib:header_value("transfer-encoding",
            headers(FirstResponse))),
    {ok, SecondResponse} = lhttpc:request(URL, get, [], 50),
    ?assertEqual({200, "OK"}, status(SecondResponse)),
    ?assertEqual(<<"Again, great success!">>, body(SecondResponse)),
    ?assertEqual("ChUnKeD", lhttpc_lib:header_value("transfer-encoding",
            headers(SecondResponse))),
    ?assertEqual("1", lhttpc_lib:header_value("Trailer-1",
            headers(SecondResponse))),
    ?assertEqual("2", lhttpc_lib:header_value("Trailer-2",
            headers(SecondResponse))).

close_connection() ->
    Port = start(gen_tcp, [fun close_connection/5]),
    URL = url(Port, "/close"),
    ?assertEqual({error, connection_closed}, lhttpc:request(URL, "GET", [],
            1000)).

ssl_get() ->
    Port = start(ssl, [fun simple_response/5]),
    URL = ssl_url(Port, "/simple"),
    {ok, Response} = lhttpc:request(URL, "GET", [], 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

ssl_post() ->
    Port = start(ssl, [fun copy_body/5]),
    URL = ssl_url(Port, "/simple"),
    Body = "SSL Test <o/",
    BinaryBody = list_to_binary(Body),
    {ok, Response} = lhttpc:request(URL, "POST", [], Body, 1000),
    ?assertEqual({200, "OK"}, status(Response)),
    ?assertEqual(BinaryBody, body(Response)).

ssl_chunked() ->
    Port = start(ssl, [fun chunked_response/5, fun chunked_response_t/5]),
    URL = ssl_url(Port, "/chunked"),
    {ok, FirstResponse} = lhttpc:request(URL, get, [], 50),
    ?assertEqual({200, "OK"}, status(FirstResponse)),
    ?assertEqual(<<?DEFAULT_STRING>>, body(FirstResponse)),
    ?assertEqual("chunked", lhttpc_lib:header_value("transfer-encoding",
            headers(FirstResponse))),
    {ok, SecondResponse} = lhttpc:request(URL, get, [], 50),
    ?assertEqual({200, "OK"}, status(SecondResponse)),
    ?assertEqual(<<"Again, great success!">>, body(SecondResponse)),
    ?assertEqual("ChUnKeD", lhttpc_lib:header_value("transfer-encoding",
            headers(SecondResponse))),
    ?assertEqual("1", lhttpc_lib:header_value("Trailer-1",
            headers(SecondResponse))),
    ?assertEqual("2", lhttpc_lib:header_value("Trailer-2",
            headers(SecondResponse))).

connection_count() ->
    timer:sleep(50), % give the TCP stack time to deliver messages
    ?assertEqual(0, lhttpc_manager:connection_count()).

invalid_options() ->
    ?assertError({bad_options, [{foo, bar}, bad_option]},
        lhttpc:request("http://localhost/", get, [], <<>>, 1000,
            [bad_option, {foo, bar}])).

%%% Helpers functions

simple(Method) ->
    Port = start(gen_tcp, [fun simple_response/5]),
    URL = url(Port, "/simple"),
    {ok, Response} = lhttpc:request(URL, Method, [], 1000),
    {StatusCode, ReasonPhrase} = status(Response),
    ?assertEqual(200, StatusCode),
    ?assertEqual("OK", ReasonPhrase),
    ?assertEqual(<<?DEFAULT_STRING>>, body(Response)).

url(Port, Path) ->
    "http://localhost:" ++ integer_to_list(Port) ++ Path.

ssl_url(Port, Path) ->
    "https://localhost:" ++ integer_to_list(Port) ++ Path.

status({Status, _, _}) ->
    Status.

body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
    Headers.

%%% Responders
simple_response(Module, Socket, _Request, _Headers, _Body) ->
    Module:send(
        Socket,
        "HTTP/1.1 200 OK\r\n"
        "Content-type: text/plain\r\nContent-length: 14\r\n\r\n"
        ?DEFAULT_STRING
    ).

head_response(Module, Socket, _Request, _Headers, _Body) ->
    Module:send(
        Socket,
        "HTTP/1.1 200 OK\r\n"
        "Server: Test server!\r\n\r\n"
    ).

no_content_response(Module, Socket, _Request, _Headers, _Body) ->
    Module:send(
        Socket,
        "HTTP/1.1 204 OK\r\n"
        "Server: Test server!\r\n\r\n"
    ).

empty_body(Module, Socket, _, _, _) ->
    Module:send(
        Socket,
        "HTTP/1.1 200 OK\r\n"
        "Content-type: text/plain\r\nContent-length: 0\r\n\r\n"
    ).

copy_body(Module, Socket, _, _, Body) ->
    Module:send(
        Socket,
        [
            "HTTP/1.1 200 OK\r\n"
            "Content-type: text/plain\r\nContent-length: "
            ++ integer_to_list(size(Body)) ++ "\r\n\r\n",
            Body
        ]
    ).

respond_and_close(Module, Socket, _, _, Body) ->
    Pid = list_to_pid(binary_to_list(Body)),
    Module:send(
        Socket,
        "HTTP/1.1 200 OK\r\n"
        "Connection: close\r\n"
        "Content-type: text/plain\r\nContent-length: 14\r\n\r\n"
        ?DEFAULT_STRING
    ),
    {error, closed} = Module:recv(Socket, 0),
    Pid ! closed,
    Module:close(Socket).

respond_and_wait(Module, Socket, _, _, Body) ->
    Pid = list_to_pid(binary_to_list(Body)),
    Module:send(
        Socket,
        "HTTP/1.1 200 OK\r\n"
        "Content-type: text/plain\r\nContent-length: 14\r\n\r\n"
        ?DEFAULT_STRING
    ),
    % We didn't signal a connection close, but we want the client to do that
    % any way
    {error, closed} = Module:recv(Socket, 0),
    Pid ! closed,
    Module:close(Socket).

pre_1_1_server(Module, Socket, _, _, Body) ->
    Pid = list_to_pid(binary_to_list(Body)),
    Module:send(
        Socket,
        "HTTP/1.0 200 OK\r\n"
        "Content-type: text/plain\r\nContent-length: 14\r\n\r\n"
        ?DEFAULT_STRING
    ),
    % We didn't signal a connection close, but we want the client to do that
    % any way since we're 1.0 now
    {error, closed} = Module:recv(Socket, 0),
    Pid ! closed,
    Module:close(Socket).

very_slow_response(Module, Socket, _, _, _) ->
    timer:sleep(1000),
    Module:send(
        Socket,
        "HTTP/1.1 200 OK\r\n"
        "Content-type: text/plain\r\nContent-length: 14\r\n\r\n"
        ?DEFAULT_STRING
    ).

no_content_length(Module, Socket, _, _, _) ->
    Module:send(
        Socket,
        "HTTP/1.1 200 OK\r\n"
        "Content-type: text/plain\r\nConnection: close\r\n\r\n"
        ?DEFAULT_STRING
    ).

no_content_length_1_0(Module, Socket, _, _, _) ->
    Module:send(
        Socket,
        "HTTP/1.0 200 OK\r\n"
        "Content-type: text/plain\r\n\r\n"
        ?DEFAULT_STRING
    ).

chunked_response(Module, Socket, _, _, _) ->
    Module:send(
        Socket,
        "HTTP/1.1 200 OK\r\n"
        "Content-type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n"
        "5\r\n"
        "Great\r\n"
        "1\r\n"
        " \r\n"
        "8\r\n"
        "success!\r\n"
        "0\r\n"
        "\r\n"
    ).

chunked_response_t(Module, Socket, _, _, _) ->
    Module:send(
        Socket,
        "HTTP/1.1 200 OK\r\n"
        "Content-type: text/plain\r\nTransfer-Encoding: ChUnKeD\r\n\r\n"
        "7\r\n"
        "Again, \r\n"
        "E\r\n"
        "great success!\r\n"
        "0\r\n"
        "Trailer-1: 1\r\n"
        "Trailer-2: 2\r\n"
        "\r\n"
    ).

close_connection(Module, Socket, _, _, _) ->
    Module:send(
        Socket,
        "HTTP/1.1 200 OK\r\n"
        "Content-type: text/plain\r\nContent-length: 14\r\n\r\n"
    ),
    Module:close(Socket).
