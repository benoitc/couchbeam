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
%%% @doc Main interface to the lightweight http client.
%%% See {@link request/4}, {@link request/5} and {@link request/6} functions.
%%% @end
-module(lhttpc).
-behaviour(application).

-export([request/4, request/5, request/6, request/9]).
-export([start/2, stop/1]).

-include("lhttpc_types.hrl").

-type result() :: {ok, {{pos_integer(), string()}, headers(), binary()}} |
    {error, atom()}.

%% @hidden
-spec start(normal | {takeover, node()} | {failover, node()}, any()) ->
    {ok, pid()}.
start(_, _) ->
    case lists:member({seed,1}, ssl:module_info(exports)) of
        true ->
            % Make sure that the ssl random number generator is seeded
            % This was new in R13 (ssl-3.10.1 in R13B vs. ssl-3.10.0 in R12B-5)
            ssl:seed(crypto:rand_bytes(255));
        false ->
            ok
    end,
    lhttpc_sup:start_link().

%% @hidden
-spec stop(any()) -> ok.
stop(_) ->
    ok.

%% @spec (URL, Method, Hdrs, Timeout) -> Result
%%   URL = string()
%%   Method = string() | atom()
%%   Hdrs = [{Header, Value}]
%%   Header = string() | binary() | atom()
%%   Value = string() | binary()
%%   Timeout = integer() | infinity
%%   Result = {ok, {{StatusCode, ReasonPhrase}, Hdrs, ResponseBody}}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ReasonPhrase = string()
%%   ResponseBody = binary()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a request without a body.
%% Would be the same as calling `request(URL, Method, Hdrs, [], Timeout)',
%% that is {@link request/5} with an empty body (`Body' could also be `<<>>').
%% @end
-spec request(string(), string() | atom(), headers(), pos_integer() |
        infinity) -> result().
request(URL, Method, Hdrs, Timeout) ->
    request(URL, Method, Hdrs, [], Timeout, []).

%% @spec (URL, Method, Hdrs, RequestBody, Timeout) -> Result
%%   URL = string()
%%   Method = string() | atom()
%%   Hdrs = [{Header, Value}]
%%   Header = string() | binary() | atom()
%%   Value = string() | binary()
%%   RequestBody = iolist()
%%   Timeout = integer() | infinity
%%   Result = {ok, {{StatusCode, ReasonPhrase}, Hdrs, ResponseBody}}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ReasonPhrase = string()
%%   ResponseBody = binary()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a request with a body.
%% Would be the same as calling
%% `request(URL, Method, Hdrs, Body, Timeout, [])', that is {@link request/6} with
%% no options.
%% @end
-spec request(string(), string() | atom(), headers(), iolist(),
        pos_integer() | infinity) -> result().
request(URL, Method, Hdrs, Body, Timeout) ->
    request(URL, Method, Hdrs, Body, Timeout, []).

%% @spec (URL, Method, Hdrs, RequestBody, Timeout, Options) -> Result
%%   URL = string()
%%   Method = string() | atom()
%%   Hdrs = [{Header, Value}]
%%   Header = string() | binary() | atom()
%%   Value = string() | binary()
%%   RequestBody = iolist()
%%   Timeout = integer() | infinity
%%   Options = [Option]
%%   Option = {connect_timeout, Milliseconds | infinity} |
%%            {send_retry, integer()}
%%   Milliseconds = integer()
%%   Result = {ok, {{StatusCode, ReasonPhrase}, Hdrs, ResponseBody}}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ReasonPhrase = string()
%%   ResponseBody = binary()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a request with a body.
%% Would be the same as calling <pre>
%% {Host, Port, Path, Ssl} = lhttpc_lib:parse_url(URL),
%% request(Host, Port, Path, Ssl, Method, Hdrs, Body, Timeout, Options).
%% </pre>
%%
%% `URL' is expected to be a valid URL: 
%% `scheme://host[:port][/path]'.
%% @end
-spec request(string(), string() | atom(), headers(), iolist(),
        pos_integer() | infinity, [option()]) -> result().
request(URL, Method, Hdrs, Body, Timeout, Options) ->
    {Host, Port, Path, Ssl} = lhttpc_lib:parse_url(URL),
    request(Host, Port, Ssl, Path, Method, Hdrs, Body, Timeout, Options).

%% @spec (Host, Port, Ssl, Path, Method, Hdrs, RequestBody, Timeout, Options) ->
%%                                                                        Result
%%   Host = string()
%%   Port = integer()
%%   Ssl = boolean()
%%   Path = string()
%%   Method = string() | atom()
%%   Hdrs = [{Header, Value}]
%%   Header = string() | binary() | atom()
%%   Value = string() | binary()
%%   RequestBody = iolist()
%%   Timeout = integer() | infinity
%%   Options = [Option]
%%   Option = {connect_timeout, Milliseconds | infinity} |
%%            {send_retry, integer()}
%%   Milliseconds = integer()
%%   Result = {ok, {{StatusCode, ReasonPhrase}, Hdrs, ResponseBody}}
%%            | {error, Reason}
%%   StatusCode = integer()
%%   ReasonPhrase = string()
%%   ResponseBody = binary()
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a request with a body.
%%
%% Instead of building and parsing URLs the target server is specified with
%% a host, port, weither SSL should be used or not and a path on the server.
%% For instance, if you want to request http://example.com/foobar you would
%% use the following:<br/>
%% `Host' = `"example.com"'<br/>
%% `Port' = `80'<br/>
%% `Ssl' = `false'<br/>
%% `Path' = `"/foobar"'<br/>
%% `Path' must begin with a forward slash `/'.
%% 
%% `Method' is either a string, stating the HTTP method exactly as in the
%% protocol, i.e: `"POST"' or `"GET"'. It could also be an atom, which is
%% then made in to uppercase, if it isn't already.
%% `Hdrs' is a list of headers to send. Mandatory headers such as
%% `Host' or `Content-Length' (for some requests) are added.
%% `Body' is the entity to send in the request. Please don't include entity
%% bodies where there shouldn't be any (such as for `GET').
%% `Timeout' is the timeout for the request in milliseconds.
%% `Options' is a list of options.
%%
%% Options:
%%
%% `{connect_timeout, Milliseconds}' specifies how many milliseconds the
%% client can spend trying to establish a connection to the server. This
%% doesn't affect the overall request timeout. However, if it's longer than
%% the overall timeout it will be ignored. Also note that the TCP layer my
%% choose to give up earlier than the connect timeout, in which case the
%% client will also give up. The default value is infinity, which means that
%% it will either give up when the TCP stack gives up, or when the overall
%% request timeout is reached. 
%%
%% `{send_retry, N}' specifies how many times the client should retry
%% sending a request if the connection is closed after the data has been
%% sent. The default value is 1.
%% @end
-spec request(string(), 1..65535, true | false, string(), atom() | string(),
    headers(), iolist(), pos_integer(), [option()]) -> result().
request(Host, Port, Ssl, Path, Method, Hdrs, Body, Timeout, Options) ->
    verify_options(Options, []),
    Args = [self(), Host, Port, Ssl, Path, Method, Hdrs, Body, Options],
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
            kill_client(Pid)
    end.

kill_client(Pid) ->
    Monitor = erlang:monitor(process, Pid),
    unlink(Pid), % or we'll kill ourself :O
    exit(Pid, timeout),
    receive
        {response, Pid, R} ->
            erlang:demonitor(Monitor, [flush]),
            R;
        {'DOWN', _, process, Pid, timeout} ->
            {error, timeout};
        {'DOWN', _, process, Pid, Reason}  ->
            erlang:error(Reason)
    end.

-spec verify_options(options(), options()) -> ok.
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

-spec bad_options(options()) -> no_return().
bad_options(Errors) ->
    erlang:error({bad_options, Errors}).
