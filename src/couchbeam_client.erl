%% @author Benoît Chesneau <benoitc@e-engura.org>
%% @copyright 2009 Benoît Chesneau.
%%
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(couchbeam_client).
-author('Benoît Chesneau <benoitc@e-engura.org').

-behaviour(gen_server).

-export([start/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([send_request/6, send_request/7, body_fun/2]).

-include("couchbeam.hrl").

-define(MAX_CHUNK_SIZE, 1024*1024).
-define(IDLE_TIMEOUT, infinity).
-define(SERVER, ?MODULE).

start(Args) ->
    gen_server:start(?MODULE, Args, []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

        
init({Host, Port}) ->
    State = #couchdb_node{host=Host, port=Port},
    {ok, State}.
    
    
handle_call({request, Method, Path, Body, Headers, Params}, _From, State) ->
    R = send_request(State, Method, Path, Body, Headers, Params),
    {reply, R, State};
    
handle_call({request, Method, Path, Body, Headers, Params, Fun}, _From, State) ->
    R = send_request(State, Method, Path, Body, Headers, Params, Fun),
    {reply, R, State}.
    
send_request({Host, Port}, Method, Path, Body, Headers, Params) ->
    send_request(#couchdb_node{host=Host, port=Port}, Method, Path, Body, 
        Headers, Params, fun body_fun/2);

send_request(State, Method, Path, Body, Headers, Params) ->
    send_request(State, Method, Path, Body, Headers, 
        Params, fun body_fun/2).

send_request({Host, Port}, Method, Path, Body, Headers, Params, Fun) ->
    send_request(#couchdb_node{host=Host, port=Port}, Method, Path, Body, Headers, Params, Fun);
    
send_request(State, Method, Path, Body, Headers, Params, Fun) ->
    Method1 = convert_method(Method),
    Path1 = lists:append([Path, 
            case Params of
            [] -> [];
            Props -> "?" ++ encode_query(Props)
            end]),
    
    case Body of
    nil when Method =:= 'POST' orelse Method =:= 'PUT' -> 
        send_request(State, Method, Path, <<>>, Headers, Params, Fun);
    nil ->
        do_req(Method1, State, Path1, Body, Headers, Fun);
    _ ->
        Headers1 = default_header("Content-Type", "application/json", Headers),
        
        case make_body(Body, Headers1) of
        {ok, B, H} ->
            do_req(Method1, State, Path1, B, H, Fun);
        {error, Reason} ->
            {error, {bad_request, Reason}}
        end
    end.    
        
handle_cast(_Msg, State) ->
    {noreply, State}.    
    
handle_info(_Info, State) ->
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
terminate(_Reason, _State) ->
    ok.
    

%% Internal API
do_req(Method, State, Path, Body, Headers, Fun) ->           
    case connect_url(State) of
    {ok, Sock} ->
        case start_client_request(Sock, State, Method, Path, Headers) of
        ok ->
            case send_body(Body, Sock) of
            ok -> 
                case get_response_headers(Sock) of
                {error, R} ->{error, R};
                {R, RespHeaders} ->
                     #http_response{status=Status, phrase=Phrase} = R,
                     if
                     Status >= 400, Status == 404 ->
                         {error, not_found};
                     Status >= 400, Status == 409 ->
                         {error, conflict};
                     Status >= 400, Status == 412 ->
                         {error, precondition_failed};
                     Status >= 400 ->
                         {error, {unknown_error, Status}};
                     true ->
                         if
                         Method == "HEAD" ->
                             {ok, {Status, Phrase}};
                         true ->
                             Resp = recv_body(Sock, RespHeaders, Fun),
                             try couchbeam:json_decode(?b2l(Resp)) of
                                Resp1 -> {json, Resp1}
                             catch
                                _:_ -> {raw, Resp}
                             end
                        end
                    end
                end;
            Error ->
                {error, {bad_request, Error}}
            end;
        {error, Reason2} ->
            {error, {bad_request, Reason2}}
        end;
    {error, Reason} ->
        {error, {bad_request, Reason}}
    end.
    
    

recv_body(Sock, Headers, Fun) ->
    {WrappedFun, InitialState} = case Fun of
    {Fun1, State1} ->
        {Fun1, State1};
    _Fun ->
        {Fun, []}
    end,
    
    FunWrapper = fun(Data, Acc) ->
        WrappedFun(Data, Acc)
    end,
        
    case body_length(Headers) of
    {unknown_transfer_encoding, _} -> FunWrapper({<<>>, done}, InitialState);
    undefined -> FunWrapper({<<>>, done}, InitialState);
    0 -> FunWrapper({<<>>, done}, InitialState);
    chunked -> recv_chunked_body(Sock, ?MAX_CHUNK_SIZE, FunWrapper, InitialState);
    Length -> recv_unchunked_body(Sock, ?MAX_CHUNK_SIZE, Length, FunWrapper, InitialState)
    end.
        
body_length(H) ->
	case proplists:get_value('Transfer-Encoding', H) of
	undefined ->
		case proplists:get_value('Content-Length', H) of
		undefined -> undefined;
		Length -> list_to_integer(Length)
		end;
	"chunked" -> chunked;
    Unknown -> {unknown_transfer_encoding, Unknown}
    end.

recv_unchunked_body(Sock, MaxHunk, DataLeft, Fun, Acc) ->
    inet:setopts(Sock, [{packet, raw}]),
    case MaxHunk >= DataLeft of
    true ->
        {ok, Data1} = gen_tcp:recv(Sock, DataLeft, ?IDLE_TIMEOUT),
        Acc1 = Fun({Data1, done}, Acc),
        Acc1;
    false ->
        {ok, Data2} = gen_tcp:recv(Sock, MaxHunk, ?IDLE_TIMEOUT),
        Acc2 = Fun(Data2, Acc),
        recv_unchunked_body(Sock, MaxHunk, DataLeft-MaxHunk, Fun, Acc2)
    end.
    
recv_chunked_body(Sock, MaxChunkSize, Fun, Acc) ->
    case read_chunk_length(Sock) of
    0 ->
        Bin = read_chunk(Sock, 0),
        Acc1 = Fun({Bin, done}, Acc),
        Acc1;     
    Length ->
        recv_chunked_body(Sock, MaxChunkSize, Length, Fun, Acc)
    end.

recv_chunked_body(Sock, MaxChunkSize, LeftInChunk, Fun, Acc) ->
    case MaxChunkSize >= LeftInChunk of
    true ->
        Data1 = read_chunk(Sock, LeftInChunk),
        Acc1 = Fun({Data1, end_chunk}, Acc),
        recv_chunked_body(Sock, MaxChunkSize, Fun, Acc1);
    false ->
        {ok, Data2} = gen_tcp:recv(Sock, MaxChunkSize,
            ?IDLE_TIMEOUT),
        Acc2 = Fun(Data2, Acc),
        recv_chunked_body(Sock, MaxChunkSize, LeftInChunk-MaxChunkSize, Fun, Acc2)
    end.
    
read_chunk_length(Sock) ->
    inet:setopts(Sock, [{packet, line}]),
    case gen_tcp:recv(Sock, 0, ?IDLE_TIMEOUT) of
        {ok, Header} ->
            inet:setopts(Sock, [{packet, raw}]),
            Splitter = fun (C) ->
                               C =/= $\r andalso C =/= $\n andalso C =/= $
                       end,
            {Hex, _Rest} = lists:splitwith(Splitter, binary_to_list(Header)),
            
            %% catch badly encoded requests
            case Hex of
                [] -> 0;
                _ -> erlang:list_to_integer(Hex, 16)
            end;
        _ ->
            exit(normal)
    end.

read_chunk(Sock,  0) ->
    inet:setopts(Sock, [{packet, line}]),
    F = fun (F1, Acc) ->
                case gen_tcp:recv(Sock, 0, ?IDLE_TIMEOUT) of
                    {ok, <<"\r\n">>} ->
                        Acc;
                    {ok, Footer} ->
                        F1(F1, [Footer | Acc]);
                    _ ->
                        exit(normal)
                end
        end,
    Footers = F(F, []),
    inet:setopts(Sock, [{packet, raw}]),
    Footers;
read_chunk(Sock, Length) ->
    case gen_tcp:recv(Sock, 2 + Length, ?IDLE_TIMEOUT) of
        {ok, <<Chunk:Length/binary, "\r\n">>} ->
            Chunk;
        _ ->
            exit(normal)
    end.
    
body_fun(Data, Acc) ->
    case Data of 
    {Data1, done} ->
        iolist_to_binary(lists:reverse([Data1|Acc]));
    {Data2, end_chunk} ->
        [Data2|Acc];
    Data3 ->
        [Data3|Acc]
    end.
    
    
collect_headers (Sock, Resp, Headers, Count) when Count < 1000 ->
    case gen_tcp:recv(Sock, 0, ?IDLE_TIMEOUT) of
        {ok, {http_header, _Num, Name, _, Value}} ->
            collect_headers(Sock, Resp, [{Name, Value} | Headers], Count+1);
        {ok, http_eoh} ->
            Headers;
        {error, {http_error, "\r\n"}} ->
            collect_headers(Sock, Resp, Headers, Count+1);
        {error, {http_error, "\n"}} ->
            collect_headers(Sock, Resp, Headers, Count+1);
        _Err ->
            exit(normal) 
    end.

http_recv_request(Sock) ->
    case gen_tcp:recv(Sock, 0, ?IDLE_TIMEOUT) of
        {ok, R} when is_record(R, http_response) ->
            R;
        {error, {http_error, "\r\n"}} ->
            http_recv_request(Sock);
        {error, {http_error, "\n"}} ->
            http_recv_request(Sock);
        {error, {http_error, _}} ->
            bad_request;
        {error, closed} -> 
            closed;
        {error, timeout} -> closed;
        _Other ->
            io:format("Got ~p~n", [_Other]),
            exit(normal)
    end.

get_response_headers(Sock) ->
    inet:setopts(Sock, [{packet, http}]),
    case http_recv_request(Sock) of
        bad_request ->
            {error, {bad_request, <<"Bad request">>}};
        closed ->
            {error, {closed, <<"Remote connection closed">>}};
        R -> 
            H = collect_headers(Sock, R, [], 0),
            {R, H}
    end.
    

    
send_body(Fun, Sock) when is_function(Fun) ->
    send_body({Fun}, Sock);
send_body({Fun}, Sock) when is_function(Fun) ->
    send_body1(Fun, Fun(), Sock);
send_body({Fun, InitialState}, Sock) when is_function(Fun) ->
    send_body1(Fun, Fun(InitialState), Sock);
send_body(nil, _Sock) ->
    ok;
send_body(Body, Sock) ->
    gen_tcp:send(Sock, Body).
    
send_body1(Fun, Resp, Sock) ->
    case Resp of
    {ok, Data} ->
        gen_tcp:send(Sock, Data),
        send_body({Fun}, Sock);
    {ok, Data, NewState} ->
        gen_tcp:send(Sock, Data),
        send_body({Fun, NewState}, Sock);
    eof ->
        ok;
    Err -> 
        Err
    end.
    
make_body(B, H) when is_list(B) ->
    B1 = ?l2b(B),
    {ok, B1, default_content_length(B1, H)};
make_body(B, H) when is_binary(B) ->
    {ok, B, default_content_length(B, H)};
make_body(Fun, H) when is_function(Fun) ->
    case  proplists:get_value("Content-Length", H) of
    undefined -> {error, "Content-Length undefined"};
    _ -> {ok, Fun, H}
    end;
make_body({Fun}, H) when is_function(Fun) ->    
    case  proplists:get_value("Content-Length", H) of
    undefined -> {error, "Content-Length undefined"};
    _ -> {ok, Fun, H}
    end;
make_body({Fun, _}=Body, H) when is_function(Fun) ->    
    case proplists:get_value("Content-Length", H) of
    undefined -> {error, "Content-Length undefined"};
    _ -> {ok, Body, H}
    end.    
    
default_content_length(B, H) ->
    default_header("Content-Length", integer_to_list(erlang:iolist_size(B)), H).

connect_url(#couchdb_node{host=Host, port=Port}) ->
    gen_tcp:connect(Host, Port, [
        binary,
        {active, false}, 
        {packet, http},
        {nodelay, true}]).
        
        
%% @doc start client request by editing headers
start_client_request(Sock, #couchdb_node{host=Host, port=Port}, Method, Path, Hdrs) ->
    Hdrs1 = [{"Host",  lists:append([Host, ":", make_int(Port)])}| Hdrs],
    Hdrs2 = insert_default_headers([{"Accept", "appplication/json"}, 
                                    {"User-Agent", ?USER_AGENT}], Hdrs1),
    HStr = headers_to_str(Hdrs2),
    gen_tcp:send(Sock, [?l2b(Method), <<" ">>, ?l2b(Path), <<" HTTP/1.1 ">>, <<"\r\n">> | HStr]).

insert_default_headers([], H) ->
    H;
insert_default_headers([{K, V}|Rest], H) ->
    H1 = default_header(K, V, H),
    insert_default_headers(Rest, H1).
                    
headers_to_str(Headers) ->
    F = fun ({K, V}, Acc) ->
                [make_io(K), <<": ">>, V, <<"\r\n">> | Acc]
        end,
    lists:foldl(F, [<<"\r\n">>], Headers).
    
convert_method(Method) when is_atom(Method) ->
    atom_to_list(Method);
convert_method(Method) when is_list(Method) ->
    Method.

encode_query(Props) ->
    RevPairs = lists:foldl(fun ({K, V}, Acc) ->
                                   [[couchbeam_util:quote_plus(K), $=, 
                                   encode_query_value(K, V)] | Acc]
                           end, [], Props),
    lists:flatten(couchbeam_util:revjoin(RevPairs, $&, [])).
        
encode_query_value(K,V) ->
    V1 = case K of
    "key"-> encode_value(V);
    "startkey" -> encode_value(V);
    "endkey" -> encode_value(V);
    _ -> couchbeam_util:val(V)
    end,
    V1.

encode_value(V) ->
    V1 = couchbeam:json_encode(V),
    couchbeam_util:quote_plus(binary_to_list(iolist_to_binary(V1))).

make_io(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
make_io(Io) when is_list(Io); is_binary(Io) ->
    Io.  
    
make_int(Count) when is_integer(Count) ->
    integer_to_list(Count);
make_int(Count) ->
    Count.    
    

default_header(K, V, H) ->
    case proplists:is_defined(K, H) of
    true -> H;
    false -> [{K, V}|H]
    end.    
    
  


