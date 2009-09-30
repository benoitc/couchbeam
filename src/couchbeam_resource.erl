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
    
    
request(State, Method, Path, Headers, Params, Body, Opts) ->
    Result = do_request(State, Method, Path, Headers, Params, Body, Opts),
    case Result of
        {ok, {{StatusCode, ReasonPhrase}, _Hdrs, ResponseBody}} ->
            if
                StatusCode >= 400, StatusCode == 404 ->
                     {error, not_found};
                StatusCode >= 400, StatusCode == 409 ->
                     {error, conflict};
                StatusCode >= 400, StatusCode == 412 ->
                     {error, precondition_failed};
                StatusCode >= 400 ->
                     {error, {unknown_error, StatusCode}};
                true ->
                    if
                        Method == "HEAD" ->
                            {ok, {StatusCode, ReasonPhrase}};
                        true ->
                            try couchbeam:json_decode(?b2l(ResponseBody)) of
                                Resp1 -> 
                                    case Resp1 of
                                        {[{<<"ok">>, true}]} -> ok;
                                        {[{<<"ok">>, true}|Res]} -> {ok, {Res}};
                                        Obj -> {ok, Obj}
                                    end
                            catch
                                _:_ -> {ok, ResponseBody}
                            end
                    end
            end;
        _ -> Result
    end.  
    
do_request(#couchdb_params{host=Host, port=Port, ssl=Ssl, timeout=Timeout}=State, 
        Method, Path, Headers, Params, Body, Opts) ->
    verify_options(Opts, []),
    Path1 = lists:append([Path, 
            case Params of
            [] -> [];
            Props -> "?" ++ encode_query(Props)
            end]),
    Headers1 = make_auth(State, Headers),
    Headers2 = default_header("Content-Type", "application/json", Headers1),
    Args = [self(), Host, Port, Ssl, Path1, Method, Headers2, Body, Opts],
    Pid = spawn_link(lhttpc_client, request, Args),
    receive
        {response, Pid, R} ->
            R;
        {exit, Pid, Reason} ->
            % We would rather want to exit here, instead of letting the
            % linked client send us an exit signal, since this can be
            % caught by the caller.
            exit(Reason);
        {'EXIT', Pid, Reason} ->
            % This could happen if the process we're running in taps exits
            % and the client process exits due to some exit signal being
            % sent to it. Very unlikely though
            exit(Reason)
    after Timeout ->
            lhttpc:kill_client(Pid)
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
    

default_header(K, V, H) ->
    case proplists:is_defined(K, H) of
    true -> H;
    false -> [{K, V}|H]
    end.
    
    
verify_options([{send_retry, N} | Options], Errors)
        when is_integer(N), N >= 0 ->
    verify_options(Options, Errors);
verify_options([{connect_timeout, infinity} | Options], Errors) ->
    verify_options(Options, Errors);
verify_options([{connect_timeout, MS} | Options], Errors)
        when is_integer(MS), MS >= 0 ->
    verify_options(Options, Errors);
verify_options([Option | Options], Errors) ->
    verify_options(Options, [Option | Errors]);
verify_options([], []) ->
    ok;
verify_options([], Errors) ->
    bad_options(Errors).

bad_options(Errors) ->
    erlang:error({bad_options, Errors}).