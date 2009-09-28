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

%%% @private
%%% @author Oscar Hellström <oscar@erlang-consulting.com>
%%% @doc
%%% This module implements the HTTP request handling. This should normally
%%% not be called directly since it should be spawned by the lhttpc module.
%%% @end
-module(lhttpc_client).

-export([request/9]).

-include("lhttpc_types.hrl").

-record(client_state, {
        host :: string(),
        port = 80 :: integer(),
        ssl = false :: true | false,
        method :: string(),
        request :: iolist(),
        request_headers :: headers(),
        socket,
        connect_timeout = infinity :: timeout(),
        attempts :: integer()
    }).

-define(CONNECTION_HDR(HDRS, DEFAULT),
    string:to_lower(lhttpc_lib:header_value("connection", HDRS, DEFAULT))).

-spec request(pid(), string(), 1..65535, true | false, string(),
        string() | atom(), headers(), iolist(), [option()]) -> no_return().
%% @spec (From, Host, Port, Ssl, Path, Method, Hdrs, RequestBody, Options) -> ok
%%    From = pid()
%%    Host = string()
%%    Port = integer()
%%    Ssl = boolean()
%%    Method = atom() | string()
%%    Hdrs = [Header]
%%    Header = {string() | atom(), string()}
%%    Body = iolist()
%%    Options = [Option]
%%    Option = {connect_timeout, Milliseconds}
%% @end
request(From, Host, Port, Ssl, Path, Method, Hdrs, Body, Options) ->
    Result = try
        execute(Host, Port, Ssl, Path, Method, Hdrs, Body, Options)
    catch 
        Reason ->
            {response, self(), {error, Reason}};
        error:closed ->
            {response, self(), {error, connection_closed}};
        error:Error ->
            {exit, self(), {Error, erlang:get_stacktrace()}}
    end,
    From ! Result,
    % Don't send back {'EXIT', self(), normal} if the process
    % calling us is trapping exits
    unlink(From), 
    
    ok.

execute(Host, Port, Ssl, Path, Method, Hdrs, Body, Options) ->
    NormalizedMethod = lhttpc_lib:normalize_method(Method),
    Request = lhttpc_lib:format_request(Path, NormalizedMethod, Hdrs, Host,
        Body),
    SocketRequest = {socket, self(), Host, Port, Ssl},
    Socket = case gen_server:call(lhttpc_manager, SocketRequest, infinity) of
        {ok, S}   -> S; % Re-using HTTP/1.1 connections
        no_socket -> undefined % Opening a new HTTP/1.1 connection
    end,
    State = #client_state{
        host = Host,
        port = Port,
        ssl = Ssl,
        method = NormalizedMethod,
        request = Request,
        request_headers = Hdrs,
        socket = Socket,
        connect_timeout = proplists:get_value(connect_timeout, Options,
            infinity),
        attempts = 1 + proplists:get_value(send_retry, Options, 1)
    },
    Response = case send_request(State) of
        {R, undefined} ->
            {ok, R};
        {R, NewSocket} ->
            % The socket we ended up doing the request over is returned
            % here, it might be the same as Socket, but we don't know.
            % I've noticed that we don't want to give send sockets that we
            % can't change the controlling process for to the manager. This
            % really shouldn't fail, but it could do if:
            % * The socket was closed remotely already 
            % * Due to an error in this module (returning dead sockets for
            %   instance)
            ManagerPid = whereis(lhttpc_manager),
            case lhttpc_sock:controlling_process(NewSocket, ManagerPid, Ssl) of
                ok ->
                    gen_server:cast(lhttpc_manager,
                        {done, Host, Port, Ssl, NewSocket});
                _ ->
                    ok
            end,
            {ok, R}
    end,
    {response, self(), Response}.

send_request(#client_state{attempts = 0}) ->
    % Don't try again if the number of allowed attempts is 0.
    throw(connection_closed);
send_request(#client_state{socket = undefined} = State) ->
    Host = State#client_state.host,
    Port = State#client_state.port,
    Ssl = State#client_state.ssl,
    Timeout = State#client_state.connect_timeout,
    SocketOptions = [binary, {packet, http}, {active, false}],
    case lhttpc_sock:connect(Host, Port, SocketOptions, Timeout, Ssl) of
        {ok, Socket} ->
            send_request(State#client_state{socket = Socket});
        {error, etimedout} ->
            % TCP stack decided to give up
            throw(connect_timeout);
        {error, timeout} ->
            throw(connect_timeout);
        {error, Reason} ->
            erlang:error(Reason)
    end;
send_request(State) ->
    Socket = State#client_state.socket,
    Ssl = State#client_state.ssl,
    Request = State#client_state.request,
    case lhttpc_sock:send(Socket, Request, Ssl) of
        ok ->
            lhttpc_sock:setopts(Socket, [{packet, http}], Ssl),
            read_response(State, nil, nil, [], <<>>);
        {error, closed} ->
            lhttpc_sock:close(Socket, Ssl),
            NewState = State#client_state{
                socket = undefined,
                attempts = State#client_state.attempts - 1
            },
            send_request(NewState);
        {error, Reason} ->
            lhttpc_sock:close(Socket, Ssl),
            erlang:error(Reason)
    end.

read_response(State, Vsn, Status, Hdrs, Body) ->
    Socket = State#client_state.socket,
    Ssl = State#client_state.ssl,
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, {http_response, NewVsn, StatusCode, Reason}} ->
            NewStatus = {StatusCode, Reason},
            read_response(State, NewVsn, NewStatus, Hdrs, Body);
        {ok, {http_header, _, Name, _, Value}} ->
            Header = {lhttpc_lib:maybe_atom_to_list(Name), Value},
            read_response(State, Vsn, Status, [Header | Hdrs], Body);
        {ok, http_eoh} ->
            lhttpc_sock:setopts(Socket, [{packet, raw}], Ssl),
            Method = State#client_state.method,
            {NewBody, NewHdrs} = maybe_read_body(Method, element(1, Status),
                Vsn, Hdrs, Ssl, Socket),
            Response = {Status, NewHdrs, NewBody},
            RequestHdrs = State#client_state.request_headers,
            NewSocket = maybe_close_socket(Socket, Ssl, Vsn, RequestHdrs,
                NewHdrs),
            {Response, NewSocket};
        {error, closed} ->
            % Either we only noticed that the socket was closed after we
            % sent the request, the server closed it just after we put
            % the request on the wire or the server has some issues and is
            % closing connections without sending responses.
            % If this the first attempt to send the request, we will try again.
            lhttpc_sock:close(Socket, Ssl),
            NewState = State#client_state{
                socket = undefined,
                attempts = State#client_state.attempts - 1
            },
            send_request(NewState);
        {error, Reason} ->
            erlang:error(Reason)
    end.

maybe_read_body("HEAD", _, _, Hdrs, _, _) ->
    % HEAD responses aren't allowed to include a body
    {<<>>, Hdrs};
maybe_read_body("OPTIONS", _, Vsn, Hdrs, Ssl, Socket) ->
    % OPTIONS can include a body, if Content-Length or Transfer-Encoding
    % indicates it.
    ContentLength = lhttpc_lib:header_value("content-length", Hdrs),
    TransferEncoding = lhttpc_lib:header_value("transfer-encoding", Hdrs),
    case {ContentLength, TransferEncoding} of
        {undefined, undefined} -> {<<>>, Hdrs};
        {_, _}                 -> read_body(Vsn, Hdrs, Ssl, Socket)
    end;
maybe_read_body(_, StatusCode, Vsn, Hdrs, Ssl, Socket) ->
    % All other requests should have a body, unless indicated otherwise 
    case StatusCode of
        204 -> {<<>>, Hdrs};
        _   -> read_body(Vsn, Hdrs, Ssl, Socket)
    end.

read_body(Vsn, Hdrs, Ssl, Socket) ->
    % Find out how to read the entity body from the request.
    % * If we have a Content-Length, just use that and read the complete
    %   entity.
    % * If Transfer-Encoding is set to chunked, we should read one chunk at
    %   the time
    % * If neither of this is true, we need to read until the socket is
    %   closed (AFAIK, this was common in versions before 1.1).
    case lhttpc_lib:header_value("content-length", Hdrs) of
        undefined ->
            TransferEncoding = string:to_lower(
                lhttpc_lib:header_value("transfer-encoding", Hdrs, "undefined")
            ),
            case TransferEncoding of
                "chunked" -> read_chunked_body(Socket, Ssl, Hdrs, []);
                _         -> read_infinite_body(Socket, Vsn, Hdrs, Ssl)
            end;
        ContentLength ->
            read_length(Hdrs, Ssl, Socket, list_to_integer(ContentLength))
    end.

read_length(Hdrs, Ssl, Socket, Length) ->
    case lhttpc_sock:recv(Socket, Length, Ssl) of
        {ok, Data} ->
            {Data, Hdrs};
        {error, Reason} ->
            erlang:error(Reason)
    end.

read_chunked_body(Socket, Ssl, Hdrs, Chunks) ->
    lhttpc_sock:setopts(Socket, [{packet, line}], Ssl),
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, ChunkSizeExt} ->
            case chunk_size(ChunkSizeExt) of
                0 ->
                    Body = list_to_binary(lists:reverse(Chunks)),
                    lhttpc_sock:setopts(Socket, [{packet, httph}], Ssl),
                    {Body, read_trailers(Socket, Ssl, Hdrs)};
                Size ->
                    Chunk = read_chunk(Socket, Ssl, Size),
                    read_chunked_body(Socket, Ssl, Hdrs, [Chunk | Chunks])
            end;
        {error, Reason} ->
            erlang:error(Reason)
    end.

chunk_size(Bin) ->
    erlang:list_to_integer(lists:reverse(chunk_size(Bin, [])), 16).

chunk_size(<<$;, _/binary>>, Chars) ->
    Chars;
chunk_size(<<"\r\n", _/binary>>, Chars) ->
    Chars;
chunk_size(<<$\s, Binary/binary>>, Chars) ->
    %% Facebook's HTTP server returns a chunk size like "6  \r\n"
    chunk_size(Binary, Chars);
chunk_size(<<Char, Binary/binary>>, Chars) ->
    chunk_size(Binary, [Char | Chars]).

read_chunk(Socket, Ssl, Size) ->
    lhttpc_sock:setopts(Socket, [{packet, raw}], Ssl),
    case lhttpc_sock:recv(Socket, Size + 2, Ssl) of
        {ok, <<Chunk:Size/binary, "\r\n">>} ->
            Chunk;
        {ok, Data} ->
            erlang:error({invalid_chunk, Data});
        {error, Reason} ->
            erlang:error(Reason)
    end.

read_trailers(Socket, Ssl, Hdrs) ->
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, http_eoh} ->
            Hdrs;
        {ok, {http_header, _, Name, _, Value}} ->
            Header = {lhttpc_lib:maybe_atom_to_list(Name), Value},
            read_trailers(Socket, Ssl, [Header | Hdrs]);
        {error, {http_error, Data}} ->
            erlang:error({bad_trailer, Data})
    end.

read_infinite_body(Socket, {1, Minor}, Hdrs, Ssl) when Minor >= 1 ->
    HdrValue = lhttpc_lib:header_value("connection", Hdrs, "keep-alive"),
    case string:to_lower(HdrValue) of
        "close" -> read_until_closed(Socket, <<>>, Hdrs, Ssl);
        _       -> erlang:error(no_content_length)
    end;
read_infinite_body(Socket, _, Hdrs, Ssl) ->
    HdrValue = lhttpc_lib:header_value("connection", Hdrs, "close"),
    case string:to_lower(HdrValue) of
        "keep-alive" -> erlang:error(no_content_length);
        _            -> read_until_closed(Socket, <<>>, Hdrs, Ssl)
    end.

read_until_closed(Socket, Acc, Hdrs, Ssl) ->
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, Body} ->
            NewAcc = <<Acc/binary, Body/binary>>,
            read_until_closed(Socket, NewAcc, Hdrs, Ssl);
        {error, closed} ->
            {Acc, Hdrs};
        {error, Reason} ->
            erlang:error(Reason)
    end.

maybe_close_socket(Socket, Ssl, {1, Minor}, ReqHdrs, RespHdrs) when Minor >= 1->
    ClientConnection = ?CONNECTION_HDR(ReqHdrs, "keep-alive"),
    ServerConnection = ?CONNECTION_HDR(RespHdrs, "keep-alive"),
    if
        ClientConnection =:= "close"; ServerConnection =:= "close" ->
            lhttpc_sock:close(Socket, Ssl),
            undefined;
        ClientConnection =/= "close", ServerConnection =/= "close" ->
            Socket
    end;
maybe_close_socket(Socket, Ssl, _, ReqHdrs, RespHdrs) ->
    ClientConnection = ?CONNECTION_HDR(ReqHdrs, "keep-alive"),
    ServerConnection = ?CONNECTION_HDR(RespHdrs, "close"),
    if
        ClientConnection =:= "close"; ServerConnection =/= "keep-alive" ->
            lhttpc_sock:close(Socket, Ssl),
            undefined;
        ClientConnection =/= "close", ServerConnection =:= "keep-alive" ->
            Socket
    end.
