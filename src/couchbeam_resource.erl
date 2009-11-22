%%% Copyright 2009 Benoît Chesneau.
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%% @author Benoît Chesneau <benoitc@e-engura.org>
%% @copyright 2009 Benoît Chesneau.

-module(couchbeam_resource).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-include("couchbeam.hrl").


-export([get/5, head/5, delete/5, post/6, put/6]).
-export([get_body_part/1,get_body_part/2]).
-export([encode_query/1]).

-record(response, {
    method,
    status,
    reason,
    headers,
    body
}).

get(State, Path, Headers, Params, Opts) ->
    request(State, "GET", Path, Headers, Params, [], Opts).

head(State, Path, Headers, Params, Opts) ->
    request(State, "HEAD", Path, Headers, Params, [], Opts).
    
delete(State, Path, Headers, Params, Opts) ->
    request(State, "DELETE", Path, Headers, Params, [], Opts).

post(State, Path, Headers, Params, Body, Opts) ->
    request(State, "POST", Path, Headers, Params, Body, Opts).

put(State, Path, Headers, Params, Body, Opts) ->
    request(State, "PUT", Path, Headers, Params, Body, Opts).
    
    
request(State, Method, Path, Headers, Params, Body, Options) ->
    Path1 = lists:append([Path, 
            case Params of
            [] -> [];
            Props -> "?" ++ encode_query(Props)
            end]),
    Headers1 = make_auth(State, Headers),
    Headers2 = default_header("Content-Type", "application/json", Headers1),
        
     case has_body(Method) of
            true ->
                case make_body(Body, Headers, Options) of
                    {Headers3, Options1, InitialBody, BodyFun} ->
                        do_request(State, Method, Path1, Headers3, {BodyFun, InitialBody}, Options1);
                    Error ->
                        Error
                end;
            false ->
                do_request(State, Method, Path1, Headers2, {nil, <<>>}, Options)
    end.
    

do_request(#couchdb_params{host=Host, port=Port, ssl=Ssl, timeout=Timeout},
        Method, Path, Headers, {BodyFun, InitialBody}, Options) ->
    case lhttpc:request(Host, Port, Ssl, Path, Method, Headers, InitialBody, Timeout, Options) of
        {ok, {{StatusCode, ReasonPhrase}, ResponseHeaders, ResponseBody}} ->
            State = #response{method    = Method,
                              status    = StatusCode,
                              reason    = ReasonPhrase, 
                              headers   = ResponseHeaders,
                              body      = ResponseBody},
                              
            make_response(State);
        {ok, UploadState} -> %% we stream
            case stream_body(BodyFun, UploadState) of
                {ok, {{StatusCode, ReasonPhrase}, ResponseHeaders, ResponseBody}} ->
                    State = #response{method    = Method,
                                      status    = StatusCode,
                                      reason    = ReasonPhrase, 
                                      headers   = ResponseHeaders,
                                      body      = ResponseBody},

                    make_response(State);
                Error -> Error
            end;
        Error -> Error
    end.  
    
make_response(#response{method=Method, status=Status, reason=Reason, body=Body}) ->
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
                    {ok, {Status, Reason}};
                true ->
                    case is_pid(Body) of
                        true ->
                            {ok, Body};
                        false ->
                            try couchbeam:json_decode(binary_to_list(Body)) of
                                Resp1 -> 
                                    case Resp1 of
                                        {[{<<"ok">>, true}]} -> ok;
                                        {[{<<"ok">>, true}|Res]} -> {ok, {Res}};
                                        Obj -> {ok, Obj}
                                    end
                            catch
                                _:_ -> {ok, Body}
                            end
                    end
            end
    end.

make_auth(#couchdb_params{username=nil, password=nil}, Headers) ->
    Headers;
make_auth(#couchdb_params{username=_UserName, password=nil}=State, Headers) ->
    make_auth(State#couchdb_params{password=""}, Headers);
make_auth(#couchdb_params{username=UserName, password=Password}, Headers) ->
    default_header("Authorization", "Basic " ++
          base64:encode_to_string(UserName ++ ":" ++ Password), Headers).
    
encode_query(Props) ->
    RevPairs = lists:foldl(fun ({K, V}, Acc) ->
                                   [[couchbeam_util:quote_plus(K), $=, 
                                   encode_query_value(K, V)] | Acc]
                           end, [], Props),
    lists:flatten(couchbeam_util:revjoin(RevPairs, $&, [])).
  
encode_query_value(K,V) when is_atom(K) ->
    encode_query_value(atom_to_list(K), V);
encode_query_value(K,V) when is_binary(K) ->
    encode_query_value(binary_to_list(K), V);
encode_query_value(K,V) ->
    V1 = case K of
    "key"-> encode_value(V);
    "startkey" -> encode_value(V);
    "endkey" -> encode_value(V);
    _ -> 
        couchbeam_util:quote_plus(V)
    end,
    V1.

encode_value(V) ->
    V1 = couchbeam:json_encode(V),
    couchbeam_util:quote_plus(binary_to_list(iolist_to_binary(V1))).
    
    
default_header(K, V, H) ->
    case proplists:is_defined(K, H) of
    true -> H;
    false -> [{K, V}|H]
    end.
    
has_body("HEAD") ->
    false;
has_body("GET") ->
    false;
has_body("DELETE") ->
    false;
has_body(_) ->
    true.
    
default_content_length(B, H) ->
    default_header("Content-Length", integer_to_list(erlang:iolist_size(B)), H).

body_length(H) ->
    case proplists:get_value("Content-Length", H) of
        undefined -> false;
        _ -> true
    end.
    
make_body(Body, Headers, Options) when is_list(Body) ->
    {default_content_length(Body, Headers), Options, Body, nil};
make_body(Body, Headers, Options) when is_binary(Body) ->
    {default_content_length(Body, Headers), Options, Body, nil};
make_body(Fun, Headers, Options) when is_function(Fun) ->
    case body_length(Headers) of
        true ->
            {ok, InitialState} = Fun(),
            Options1 = [{partial_upload, infinity}|Options],
            {Headers, Options1, InitialState, Fun};
        false ->
            {error,  "Content-Length undefined"}
    end;
make_body({Fun, State}, Headers, Options) when is_function(Fun) ->
    case body_length(Headers) of
        true ->
            Options1 = [{partial_upload, infinity}|Options],
            {ok, InitialState, NextState} = Fun(State),
            
            {Headers, Options1, InitialState, {Fun, NextState}};
        false ->
            {error,  "Content-Length undefined"}
    end;
make_body(_, _, _) ->
    {error, "body invalid"}.
     
     
stream_body({Source, State}, CurrentState) ->
    do_stream_body(Source, Source(State), CurrentState);
stream_body(Source, CurrentState) ->
    do_stream_body(Source, Source(), CurrentState).
    
do_stream_body(Source, Resp, CurrentState) ->
    case Resp of
        {ok, Data} ->
            {ok, NextState} = lhttpc:send_body_part(CurrentState, Data),
            stream_body(Source, NextState);
        {ok, Data, NewSourceState} ->
            {ok, NextState} = lhttpc:send_body_part(CurrentState, Data),
            stream_body({Source, NewSourceState}, NextState);
        eof ->
            lhttpc:send_body_part(CurrentState, http_eob)
    end.
            
%% @spec (HTTPClient :: pid()) -> Result
%%   Result = {ok, Bin} | {ok, {http_eob, Trailers}} 
%%   Trailers = [{Header, Value}]
%%   Header = string() | binary() | atom()
%%   Value = string() | binary()
%% @doc Reads a body part from an ongoing response when
%% `Streaming` option is true in `couchbeam_db:fetch_attchment/4`. The default timeout,
%% `infinity' will be used. 
%% Would be the same as calling
%% `get_body_part(HTTPClient, infinity)'.
%% @end
-spec get_body_part(pid()) -> {ok, binary()} | {ok, {http_eob, headers()}}.
get_body_part(Pid) ->
    get_body_part(Pid, infinity).

%% @spec (HTTPClient :: pid(), Timeout:: Timeout) -> Result
%%   Timeout = integer() | infinity
%%   Result = {ok, Bin} | {ok, {http_eob, Trailers}} 
%%   Trailers = [{Header, Value}]
%%   Header = string() | binary() | atom()
%%   Value = string() | binary()
%% @doc Reads a body part from an ongoing response when
%% `Streaming` option is true in `couchbeam_db:fetch_attchment/4`.
%% `Timeout' is the timeout for reading the next body part in milliseconds. 
%% `http_eob' marks the end of the body. If there were Trailers in the
%% response those are returned with `http_eob' as well. 
%% @end
-spec get_body_part(pid(), timeout()) -> 
        {ok, binary()} | {ok, {http_eob, headers()}}.
get_body_part(Pid, Timeout) ->
    lhttpc:get_body_part(Pid, Timeout).