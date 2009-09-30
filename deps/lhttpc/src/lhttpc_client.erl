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
        attempts :: integer(),
        requester :: pid(), 
        partial_upload = false :: true | false,
        chunked_upload = false ::true | false,
        upload_window :: non_neg_integer() | infinity,
        partial_download = false :: true | false,
        download_window = infinity :: timeout(),
        part_size :: non_neg_integer() | infinity
        %% in case of infinity we read whatever data we can get from
        %% the wire at that point or 
        %% in case of chunked one chunk
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
        execute(From, Host, Port, Ssl, Path, Method, Hdrs, Body, Options)
    catch 
        Reason ->
            {response, self(), {error, Reason}};
        error:closed ->
            {response, self(), {error, connection_closed}};
        error:Error ->
            {exit, self(), {Error, erlang:get_stacktrace()}}
    end,
    case Result of
        {response, _, {ok, {no_return, _}}} -> ok;
        _Else -> From ! Result
    end,
    % Don't send back {'EXIT', self(), normal} if the process
    % calling us is trapping exits
    unlink(From), 
    ok.

execute(From, Host, Port, Ssl, Path, Method, Hdrs, Body, Options) ->
    UploadWindowSize = proplists:get_value(partial_upload, Options),
    PartialUpload = proplists:is_defined(partial_upload, Options),
    PartialDownload = proplists:is_defined(partial_download, Options),
    PartialDownloadOptions = proplists:get_value(partial_download, Options, []),
    NormalizedMethod = lhttpc_lib:normalize_method(Method),
    {ChunkedUpload, Request} = lhttpc_lib:format_request(Path, NormalizedMethod,
        Hdrs, Host, Body, PartialUpload),
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
        requester = From,
        request_headers = Hdrs,
        socket = Socket,
        connect_timeout = proplists:get_value(connect_timeout, Options,
            infinity),
        attempts = 1 + proplists:get_value(send_retry, Options, 1),
        partial_upload = PartialUpload,
        upload_window = UploadWindowSize,
        chunked_upload = ChunkedUpload,
        partial_download = PartialDownload,
        download_window = proplists:get_value(window_size, 
            PartialDownloadOptions, infinity),
        part_size = proplists:get_value(part_size,
            PartialDownloadOptions, infinity) 
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
            if
                State#client_state.partial_upload     -> partial_upload(State);
                not State#client_state.partial_upload -> read_response(State)
            end;
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

partial_upload(State) ->
    Response = {ok, {self(), State#client_state.upload_window}},
    State#client_state.requester ! {response, self(), Response},
    partial_upload_loop(State#client_state{attempts = 1, request = undefined}).

partial_upload_loop(State = #client_state{requester = Pid}) ->
    receive
        {trailers, Pid, Trailers} ->
            send_trailers(State, Trailers),
            read_response(State);
        {body_part, Pid, http_eob} ->
            send_body_part(State, http_eob),
            read_response(State);
        {body_part, Pid, Data} ->
            send_body_part(State, Data),
            Pid ! {ack, self()},
            partial_upload_loop(State)
    end.

send_body_part(State = #client_state{socket = Socket, ssl = Ssl}, BodyPart) ->
    Data = encode_body_part(State, BodyPart),
    check_send_result(State, lhttpc_sock:send(Socket, Data, Ssl)).
 
send_trailers(State = #client_state{chunked_upload = true}, Trailers) ->
    Socket = State#client_state.socket,
    Ssl = State#client_state.ssl,
    Data = [<<"0\r\n">>, lhttpc_lib:format_hdrs(Trailers)],
    check_send_result(State, lhttpc_sock:send(Socket, Data, Ssl));
send_trailers(#client_state{chunked_upload = false}, _Trailers) ->
    erlang:error(trailers_not_allowed).

encode_body_part(#client_state{chunked_upload = true}, http_eob) ->
    <<"0\r\n\r\n">>; % We don't send trailers after http_eob
encode_body_part(#client_state{chunked_upload = false}, http_eob) ->
    <<>>;
encode_body_part(#client_state{chunked_upload = true}, Data) ->
    Size = list_to_binary(erlang:integer_to_list(iolist_size(Data), 16)),
    [Size, <<"\r\n">>, Data, <<"\r\n">>];
encode_body_part(#client_state{chunked_upload = false}, Data) ->
    Data.

check_send_result(_State, ok) ->
    ok;
check_send_result(#client_state{socket = Sock, ssl = Ssl}, {error, Reason}) ->
    lhttpc_sock:close(Sock, Ssl),
    throw(Reason).
 
read_response(#client_state{socket = Socket, ssl = Ssl} = State) ->
    lhttpc_sock:setopts(Socket, [{packet, http}], Ssl),
    read_response(State, nil, nil, []).

read_response(State, Vsn, Status, Hdrs) ->
    Socket = State#client_state.socket,
    Ssl = State#client_state.ssl,
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, {http_response, NewVsn, StatusCode, Reason}} ->
            NewStatus = {StatusCode, Reason},
            read_response(State, NewVsn, NewStatus, Hdrs);
        {ok, {http_header, _, Name, _, Value}} ->
            Header = {lhttpc_lib:maybe_atom_to_list(Name), Value},
            read_response(State, Vsn, Status, [Header | Hdrs]);
        {ok, http_eoh} ->
            lhttpc_sock:setopts(Socket, [{packet, raw}], Ssl),
            Response = handle_response_body(State, Vsn, Status, Hdrs),
            NewHdrs = element(2, Response),
            ReqHdrs = State#client_state.request_headers,
            NewSocket = maybe_close_socket(Socket, Ssl, Vsn, ReqHdrs, NewHdrs),
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

handle_response_body(State = #client_state{partial_download = false},
        Vsn, Status, Hdrs) ->
    Socket = State#client_state.socket,
    Ssl = State#client_state.ssl,
    Method = State#client_state.method,
    {Body, NewHdrs} = case has_body(Method, element(1, Status), Hdrs) of
        true  -> read_body(Vsn, Hdrs, Ssl, Socket, body_type(Hdrs));
        false -> {<<>>, Hdrs}
    end,
    {Status, NewHdrs, Body};
handle_response_body(#client_state{partial_download = true} = State,
        Vsn, Status, Hdrs) ->
    Method = State#client_state.method,
    case has_body(Method, element(1, Status), Hdrs) of
        true ->
            Response = {ok, {Status, Hdrs, self()}},
            State#client_state.requester ! {response, self(), Response},
            MonRef = erlang:monitor(process, State#client_state.requester),
            Res = read_partial_body(State, Vsn, Hdrs, body_type(Hdrs)),
            erlang:demonitor(MonRef, [flush]),
            Res;
        false ->
            {Status, Hdrs, undefined}
    end.

has_body("HEAD", _, _) ->
    % HEAD responses aren't allowed to include a body
    false;
has_body("OPTIONS", _, Hdrs) ->
    % OPTIONS can include a body, if Content-Length or Transfer-Encoding
    % indicates it.
    ContentLength = lhttpc_lib:header_value("content-length", Hdrs),
    TransferEncoding = lhttpc_lib:header_value("transfer-encoding", Hdrs),
    case {ContentLength, TransferEncoding} of
        {undefined, undefined} -> false;
        {_, _}                 -> true
    end;
has_body(_, 204, _) ->
    false; % 204 No content can't have a body
has_body(_, _, _) ->
    true. % All other responses are assumed to have a body

body_type(Hdrs) ->
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
                "chunked" -> chunked;
                _         -> infinite
            end;
        ContentLength ->
            {fixed_length, list_to_integer(ContentLength)}
    end.

read_partial_body(State, _Vsn, Hdrs, chunked) ->
    read_partial_chunked_body(State, Hdrs, State#client_state.download_window, 
        0, [], 0);
read_partial_body(State, Vsn, Hdrs, infinite) ->
    check_infinite_response(Vsn, Hdrs),
    read_partial_infinite_body(State, Hdrs, State#client_state.download_window);
read_partial_body(State, _Vsn, Hdrs, {fixed_length, ContentLength}) ->
    read_partial_finite_body(State, Hdrs, ContentLength, 
        State#client_state.download_window).
    
read_body(_Vsn, Hdrs, Ssl, Socket, chunked) ->
    read_chunked_body(Socket, Ssl, Hdrs, []);
read_body(Vsn, Hdrs, Ssl, Socket, infinite) ->
    check_infinite_response(Vsn, Hdrs),
    read_infinite_body(Socket, Hdrs, Ssl);
read_body(_Vsn, Hdrs, Ssl, Socket, {fixed_length, ContentLength}) ->
    read_length(Hdrs, Ssl, Socket, ContentLength).

read_partial_finite_body(State = #client_state{}, Hdrs, 0, _Window) ->
    send_end_of_body(State, [], Hdrs);
read_partial_finite_body(State = #client_state{requester = To}, Hdrs, 
        ContentLength, 0) ->
    receive
        {ack, To} -> read_partial_finite_body(State, Hdrs, ContentLength, 1);
        {'DOWN', _, process, To, _} -> exit(normal)
    end;
read_partial_finite_body(State = #client_state{requester = To}, Hdrs, 
        ContentLength, Window) when Window >= 0->
    Bin = read_body_part(State, ContentLength),
    State#client_state.requester ! {body_part, self(), Bin},
    receive
        {ack, To} -> read_partial_finite_body(State, Hdrs, 
                        ContentLength - iolist_size(Bin), Window);
        {'DOWN', _, process, To, _} -> exit(normal)
    after 0 ->
        read_partial_finite_body(State, Hdrs, ContentLength - iolist_size(Bin), 
            lhttpc_lib:dec(Window))
    end.

read_body_part(#client_state{socket = Socket, ssl = Ssl, part_size = infinity},
        _ContentLength) ->
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, Data} ->
            Data;
        {error, Reason} ->
            erlang:error(Reason)
    end; 
read_body_part(#client_state{socket = Socket, ssl = Ssl, part_size = PartSize},
        ContentLength) when PartSize =< ContentLength ->
    case lhttpc_sock:recv(Socket, PartSize, Ssl) of
        {ok, Data} ->
            Data;
        {error, Reason} ->
            erlang:error(Reason)
    end;
read_body_part(#client_state{socket = Socket, ssl = Ssl, part_size = PartSize},
        ContentLength) when PartSize > ContentLength ->
    case lhttpc_sock:recv(Socket, ContentLength, Ssl) of
        {ok, Data} ->
            Data;
        {error, Reason} ->
            erlang:error(Reason)
    end.

read_length(Hdrs, Ssl, Socket, Length) ->
    case lhttpc_sock:recv(Socket, Length, Ssl) of
        {ok, Data} ->
            {Data, Hdrs};
        {error, Reason} ->
            erlang:error(Reason)
    end.

read_partial_chunked_body(State = #client_state{socket = Socket, ssl = Ssl, 
        part_size = PartSize}, Hdrs, Window, CurrentSize, CurrentData, 0) -> 
    case read_chunk_size(Socket, Ssl) of
    0 -> 
        send_chunked_body_part(State, CurrentData, Window, CurrentSize),
        {Trailers, NewHdrs} = read_trailers(Socket, Ssl, [], Hdrs),
        send_end_of_body(State, Trailers, NewHdrs);
    ChunkSize when PartSize =:= infinity ->
        Chunk = read_chunk(Socket, Ssl, ChunkSize),
        NewWindow = send_chunked_body_part(State, [Chunk | CurrentData], Window,
            not_empty),
        read_partial_chunked_body(State, Hdrs, NewWindow, 0, [], 0);
    ChunkSize when CurrentSize + ChunkSize >= PartSize ->
        {Chunk, RestChunkSize} = 
            read_partial_chunk(Socket, Ssl, PartSize - CurrentSize, ChunkSize),
        NewWindow = send_chunked_body_part(State, [Chunk | CurrentData], Window,
            not_empty),
        read_partial_chunked_body(State, Hdrs, NewWindow, 0, [], RestChunkSize);
    ChunkSize ->
        Chunk = read_chunk(Socket, Ssl, ChunkSize),
        read_partial_chunked_body(State, Hdrs, Window, CurrentSize + ChunkSize,
            [Chunk | CurrentData], 0)
    end;
read_partial_chunked_body(State = #client_state{socket = Socket, ssl = Ssl,
        part_size = PartSize}, Hdrs, Window, CurrentSize, CurrentData, 
        RestChunkSize) ->
    if 
        CurrentSize + RestChunkSize >= PartSize ->
            {Chunk, NewRestChunkSize} = 
                read_partial_chunk(Socket, Ssl, PartSize - CurrentSize, 
                    RestChunkSize),
            NewWindow = send_chunked_body_part(State, [Chunk | CurrentData], 
                Window, not_empty),
            read_partial_chunked_body(State, Hdrs, NewWindow, 0, [], 
                NewRestChunkSize);            
        CurrentSize + RestChunkSize < PartSize ->
            Chunk = read_chunk(Socket, Ssl, RestChunkSize),
            read_partial_chunked_body(State, Hdrs, Window, 
                CurrentSize + RestChunkSize, [Chunk | CurrentData], 0)
    end.             

read_chunk_size(Socket, Ssl) ->
    lhttpc_sock:setopts(Socket, [{packet, line}], Ssl),
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, ChunkSizeExt} ->
            chunk_size(ChunkSizeExt);
        {error, Reason} ->
            erlang:error(Reason)
    end.


send_chunked_body_part(_State, _Bin, Window, 0) -> 
    Window;
send_chunked_body_part(State = #client_state{requester = Pid}, Bin, 0, Size) ->
    receive
        {ack, Pid} -> send_chunked_body_part(State, Bin, 1, Size);
        {'DOWN', _, process, Pid, _} -> exit(normal)
    end;
send_chunked_body_part(#client_state{requester = Pid}, Bin, Window, _Size) ->
    Pid ! {body_part, self(), preformat(Bin)},
    receive
        {ack, Pid} ->  Window; 
        {'DOWN', _, process, Pid, _} -> exit(normal)
    after 0 ->
        lhttpc_lib:dec(Window)
    end.
            
preformat(Bin) when is_list(Bin) -> lists:reverse(Bin); 
%%Maybe turn it to one binary
preformat(Bin) -> Bin.                

read_chunked_body(Socket, Ssl, Hdrs, Chunks) ->
    case read_chunk_size(Socket, Ssl) of
        0 ->    
            Body = list_to_binary(lists:reverse(Chunks)),
            {Body, read_trailers(Socket, Ssl, Hdrs)};
        Size ->
            Chunk = read_chunk(Socket, Ssl, Size),
            read_chunked_body(Socket, Ssl, Hdrs, [Chunk | Chunks])
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

read_partial_chunk(Socket, Ssl, ChunkSize, ChunkSize) ->
    {read_chunk(Socket, Ssl, ChunkSize), 0};
read_partial_chunk(Socket, Ssl, Size, ChunkSize) ->
    lhttpc_sock:setopts(Socket, [{packet, raw}], Ssl),
    case lhttpc_sock:recv(Socket, Size, Ssl) of
        {ok, Chunk} ->
            {Chunk, ChunkSize - Size};
        {error, Reason} ->
            erlang:error(Reason)
    end.

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

read_trailers(Socket, Ssl, Trailers, Hdrs) ->
    lhttpc_sock:setopts(Socket, [{packet, httph}], Ssl),
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, http_eoh} ->
            {Trailers, Hdrs};
        {ok, {http_header, _, Name, _, Value}} ->
            Header = {lhttpc_lib:maybe_atom_to_list(Name), Value},
            read_trailers(Socket, Ssl, [Header | Trailers], [Header | Hdrs]);
        {error, {http_error, Data}} ->
            erlang:error({bad_trailer, Data})
    end.
    
read_trailers(Socket, Ssl, Hdrs) ->
    lhttpc_sock:setopts(Socket, [{packet, httph}], Ssl),
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, http_eoh} ->
            Hdrs;
        {ok, {http_header, _, Name, _, Value}} ->
            Header = {lhttpc_lib:maybe_atom_to_list(Name), Value},
            read_trailers(Socket, Ssl, [Header | Hdrs]);
        {error, {http_error, Data}} ->
            erlang:error({bad_trailer, Data})
    end.

send_end_of_body(#client_state{requester = Requester}, Trailers, Hdrs) ->
    Requester ! {http_eob, self(), Trailers},
    {no_return, Hdrs}.
    
read_partial_infinite_body(State = #client_state{requester = To}, Hdrs, 0) ->
    receive
        {ack, To} -> read_partial_infinite_body(State, Hdrs, 1);
        {'DOWN', _, process, To, _} -> exit(normal)
    end;
read_partial_infinite_body(State = #client_state{requester = To}, Hdrs, Window) 
        when Window >= 0 ->
    case read_infinite_body_part(State) of
        http_eob -> send_end_of_body(State, [], Hdrs);
        Bin ->
            State#client_state.requester ! {body_part, self(), Bin},
            receive
                {ack, To} -> read_partial_infinite_body(State, Hdrs, Window);
                {'DOWN', _, process, To, _} -> exit(normal)
            after 0 ->
                read_partial_infinite_body(State, Hdrs, lhttpc_lib:dec(Window))
            end
    end.

read_infinite_body_part(#client_state{socket = Socket, ssl = Ssl}) ->
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, Data} ->
            Data;
        {error, closed} ->
            http_eob;
        {error, Reason} ->
            erlang:error(Reason)
    end.

check_infinite_response({1, Minor}, Hdrs) when Minor >= 1 ->
    HdrValue = lhttpc_lib:header_value("connection", Hdrs, "keep-alive"),
    case string:to_lower(HdrValue) of
        "close" -> ok;
        _       -> erlang:error(no_content_length)
    end;
check_infinite_response(_, Hdrs) ->
    HdrValue = lhttpc_lib:header_value("connection", Hdrs, "close"),
    case string:to_lower(HdrValue) of
        "keep-alive" -> erlang:error(no_content_length);
        _            -> ok
    end.

read_infinite_body(Socket, Hdrs, Ssl) ->   
    read_until_closed(Socket, <<>>, Hdrs, Ssl).

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
