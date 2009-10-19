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
-export([
        send_body_part/2,
        send_body_part/3, 
        send_trailers/2,
        send_trailers/3
    ]).
-export([
        get_body_part/1,
        get_body_part/2
        ]).
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
%% `request(URL, Method, Hdrs, Body, Timeout, [])', that is {@link request/6}
%% with no options.
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
%%            {send_retry, integer()} | {partial_upload, WindowSize}
%%   Milliseconds = integer()
%%   WindowSize = integer() | infinity
%%   Result = {ok, {{StatusCode, ReasonPhrase}, Hdrs, ResponseBody}} |
%%            {ok, UploadState} | {error, Reason}
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
%%            {send_retry, integer()} |
%%            {partial_upload, WindowSize} |
%%            {partial_download, PartialDowloadOptions}
%%   Milliseconds = integer()
%%   WindowSize = integer()
%%   PartialDownloadOptions = [PartialDownloadOption]
%%   PartialDowloadOption = {window_size, WindowSize} |
%%                          {part_size, PartSize}
%%   PartSize = integer()
%%   Result = {ok, {{StatusCode, ReasonPhrase}, Hdrs, ResponseBody}}
%%          | {error, Reason}
%%   StatusCode = integer()
%%   ReasonPhrase = string()
%%   ResponseBody = binary() | pid() | undefined
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
%% `Host', `Content-Length' or `Transfer-Encoding' (for some requests) 
%% are added.
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
%% sent. The default value is `1'. If `{partial_upload, WindowSize}'
%% (see below) is specified, the client cannot retry after the first part
%% of the body has been sent since it doesn't keep the whole entitity body
%% in memory.
%%
%% `{partial_upload, WindowSize}' means that the body will be supplied in
%% parts to the client by the calling process. The `WindowSize' specifies how
%% many parts can be sent to the process controlling the socket before waiting
%% for an acknowledgement. This is to create a kind of internal flow control
%% if the network is slow and the process is blocked by the TCP stack. Flow
%% control is disabled if `WindowSize' is `infinity'. If `WindowSize' is an
%% integer, it must be >= 0.  If partial upload is specified and no
%% `Content-Length' is specified in `Hdrs' the client will use chunked
%% transfer encoding to send the entity body. If a content length is
%% specified, this must be the total size of the entity body.
%% The call to {@link request/6} will return `{ok, UploadState}'. The
%% `UploadState' is supposed to be used as the first argument to the {@link
%% send_body_part/2} or {@link send_body_part/3} functions to send body parts.
%% Partial upload is intended to avoid keeping large request bodies in
%% memory but can also be used when the complete size of the body isn't known
%% when the request is started.
%%
%% `{partial_download, PartialDownloadOptions}' means that the response body
%% will be supplied in parts by the client to the calling process. The partial
%% download option `{window_size, WindowSize}' specifies how many part will be
%% sent to the calling process before waiting for an acknowledgement. This is
%% to create a kind of internal flow control if the calling process is slow to
%% process the body part and the network is considerably faster. Flow control
%% is disabled if `WindowSize' is `infinity'. If `WindowSize' is an integer it
%% must be >=0. The partial download option `{part_size, PartSize}' specifies
%% the size the body parts should come in. Note however that if the body size
%% is not determinable (e.g entity body is termintated by closing the socket)
%% it will be delivered in pieces as it is read from the wire. There is no
%% caching of the body parts until the amount reaches body size. If the body
%% size is bounded (e.g `Content-Length' specified or
%% `Transfer-Encoding: chunked' specified) it will be delivered in `PartSize'
%% pieces. Note however that the last piece might be smaller than `PartSize'.
%% Size bounded entity bodies are handled the same way as unbounded ones if
%% `PartSize' is `infinity'. If `PartSize' is integer it must be >= 0.
%% If `{partial_download, PartialDownloadOptions}' is specified the 
%% `ResponseBody' is going to be a `pid()' unless the response has no body
%% (for example in case of `HEAD' requests). In that case it is going to be
%% `undefined'. 
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

%% @spec (UploadState :: UploadState, BodyPart :: BodyPart) -> Result
%%   BodyPart = iolist() | binary()
%%   Timeout = integer() | infinity
%%   Result = {error, Reason} | UploadState
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a body part to an ongoing request when
%% `{partial_upload, WindowSize}' is used. The default timeout, `infinity'
%% will be used. Notice that if `WindowSize' is infinity, this call will never
%% block.
%% Would be the same as calling
%% `send_body_part(UploadState, BodyPart, infinity)'.
%% @end
-spec send_body_part({pid(), window_size()}, binary()) -> 
        {pid(), window_size()} | result().
send_body_part({Pid, Window}, Bin) ->
    send_body_part({Pid, Window}, Bin, infinity).

%% @spec (UploadState :: UploadState, BodyPart :: BodyPart, Timeout) -> Result
%%   BodyPart = iolist() | binary()
%%   Timeout = integer() | infinity
%%   Result = {error, Reason} | UploadState
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends a body part to an ongoing request when
%% `{partial_upload, WindowSize}' is used.
%% `Timeout' is the timeout for the request in milliseconds.
%%
%% If the window size reaches 0 the call will block for at maximum Timeout
%% milliseconds. If there is no acknowledgement received during that time the
%% the request is cancelled and `{error, timeout}' is returned.
%%
%% As long as the window size is larger than 0 the function will return 
%% immediately after sending the body part to the request handling process.
%% 
%% The `BodyPart' `http_eob' signals an end of the entity body, the request
%% is considered sent and the response will be read from the socket. If
%% there is no response within `Timeout' milliseconds, the request is
%% canceled and `{error, timeout}' is returned.
%% @end
-spec send_body_part({pid(), window_size()}, binary(), timeout()) -> 
        {ok, {pid(), window_size()}} | result().
send_body_part({Pid, 0}, Bin, Timeout) 
        when is_binary(Bin), is_pid(Pid) ->
    receive
        {ack, Pid} ->
            send_body_part({Pid, 1}, Bin, Timeout);
        {response, Pid, R} ->
            R;
        {exit, Pid, Reason} ->
            exit(Reason);
        {'EXIT', Pid, Reason} ->
            exit(Reason)
    after Timeout ->
        kill_client(Pid)
    end;
send_body_part({Pid, Window}, Bin, _Timeout) 
        when Window > 0, is_binary(Bin), is_pid(Pid) ->
    Pid ! {body_part, self(), Bin},
    receive
        {ack, Pid} ->
            %% body_part ACK
            {ok, {Pid, Window}};
        {reponse, Pid, R} ->
            %% something went wrong in the client
            %% for example the connection died or
            %% the last body part has been sent 
            R;
        {exit, Pid, Reason} ->
            exit(Reason);
        {'EXIT', Pid, Reason} ->
            exit(Reason)
    after 0 ->
        {ok, {Pid, lhttpc_lib:dec(Window)}}
    end;
send_body_part({Pid, _Window}, http_eob, Timeout) when is_pid(Pid) ->
    Pid ! {body_part, self(), http_eob},
    read_response(Pid, Timeout).

%% @spec (UploadState :: UploadState, Trailers) -> Result
%%   Header = string() | binary() | atom()
%%   Value = string() | binary()
%%   Result = {ok, {{StatusCode, ReasonPhrase}, Hdrs, ResponseBody}}
%%            | {error, Reason}
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends trailers to an ongoing request when `{partial_upload,
%% WindowSize}' is used and no `Content-Length' was specified. The default
%% timout `infinity' will be used. Note that after this the request is
%% considered complete and the response will be read from the socket. 
%% Would be the same as calling
%% `send_trailers(UploadState, BodyPart, infinity)'.
%% @end
-spec send_trailers({pid(), window_size()}, headers()) -> result().
send_trailers({Pid, Window}, Trailers) ->
    send_trailers({Pid, Window}, Trailers, infinity).

%% @spec (UploadState :: UploadState, Trailers, Timeout) -> Result
%%   Trailers = [{Header, Value}]
%%   Header = string() | binary() | atom()
%%   Value = string() | binary()
%%   Timeout = integer() | infinity
%%   Result = {ok, {{StatusCode, ReasonPhrase}, Hdrs, ResponseBody}}
%%            | {error, Reason}
%%   Reason = connection_closed | connect_timeout | timeout
%% @doc Sends trailers to an ongoing request when
%% `{partial_upload, WindowSize}' is used and no `Content-Length' was
%% specified.
%% `Timeout' is the timeout for sending the trailers and reading the
%% response in milliseconds.
%%
%% Sending trailers also signals the end of the entity body, which means
%% that no more body parts, or trailers can be sent and the response to the
%% request will be read from the socket. If no response is received within
%% `Timeout' milliseconds the request is canceled and `{error, timeout}' is
%% returned.
%% @end
-spec send_trailers({pid(), window_size()}, [{string() | string()}], 
        timeout()) -> result().
send_trailers({Pid, _Window}, Trailers, Timeout)
        when is_list(Trailers), is_pid(Pid) ->
    Pid ! {trailers, self(), Trailers},
    read_response(Pid, Timeout).

%% @spec (HTTPClient :: pid()) -> Result
%%   Result = {ok, Bin} | {ok, {http_eob, Trailers}} 
%%   Trailers = [{Header, Value}]
%%   Header = string() | binary() | atom()
%%   Value = string() | binary()
%% @doc Reads a body part from an ongoing response when
%% `{partial_download, PartialDownloadOptions}' is used. The default timeout,
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
%% `{partial_download, PartialDownloadOptions}' is used.
%% `Timeout' is the timeout for reading the next body part in milliseconds. 
%% `http_eob' marks the end of the body. If there were Trailers in the
%% response those are returned with `http_eob' as well. 
%% @end
-spec get_body_part(pid(), timeout()) -> 
        {ok, binary()} | {ok, {http_eob, headers()}}.
get_body_part(Pid, Timeout) ->
    receive
        {body_part, Pid, Bin} ->
            Pid ! {ack, self()},
            {ok, Bin};
        {http_eob, Pid, Trailers} ->
            {ok, {http_eob, Trailers}}
    after Timeout ->
        kill_client(Pid)
    end.

%%% Internal functions

-spec read_response(pid(), timeout()) -> result().
read_response(Pid, Timeout) ->
    receive
        {ack, Pid} ->
            read_response(Pid, Timeout);
        {response, Pid, R} ->
            R;
        {exit, Pid, Reason} ->
            exit(Reason);
        {'EXIT', Pid, Reason} ->
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
verify_options([{partial_upload, WindowSize} | Options], Errors)
        when is_integer(WindowSize), WindowSize >= 0 ->
    verify_options(Options, Errors);
verify_options([{partial_upload, infinity} | Options], Errors)  ->
    verify_options(Options, Errors);
verify_options([{partial_download, DownloadOptions} | Options], Errors)
        when is_list(DownloadOptions) ->
    case verify_partial_download(DownloadOptions, []) of
        [] ->
            verify_options(Options, Errors);
        OptionErrors ->
            NewErrors = [{partial_download, OptionErrors} | Errors],
            verify_options(Options, NewErrors)
    end;
verify_options([Option | Options], Errors) ->
    verify_options(Options, [Option | Errors]);
verify_options([], []) ->
    ok;
verify_options([], Errors) ->
    bad_options(Errors).

-spec bad_options(options()) -> no_return().
bad_options(Errors) ->
    erlang:error({bad_options, Errors}).

-spec verify_partial_download(options(), options()) -> options().

verify_partial_download([{window_size, infinity} | Options], Errors)->
    verify_partial_download(Options, Errors);
verify_partial_download([{window_size, Size} | Options], Errors) when
        is_integer(Size), Size >= 0 ->
    verify_partial_download(Options, Errors);
verify_partial_download([{part_size, Size} | Options], Errors) when
        is_integer(Size), Size >= 0 ->
    verify_partial_download(Options, Errors);
verify_partial_download([Option | Options], Errors) ->
    verify_partial_download(Options, [Option | Errors]);
verify_partial_download([], Errors) ->
    Errors.
