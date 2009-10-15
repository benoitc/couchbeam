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
%%% This module implements various library functions used in lhttpc.
%%% @end
-module(lhttpc_lib).

-export([
        parse_url/1,
        format_request/6,
        header_value/2,
        header_value/3,
        normalize_method/1
    ]).
-export([maybe_atom_to_list/1]).

-export([format_hdrs/1, dec/1]).

-include("lhttpc_types.hrl").

%% @spec header_value(Header, Headers) -> undefined | term()
%% Header = string()
%% Headers = [{string(), term()}]
%% Value = term()
%% @doc
%% Returns the value associated with the `Header' in `Headers'.
%% `Header' must be a lowercase string, since every header is mangled to
%% check the match.
%% @end
-spec header_value(string(), [{string(), Value}]) -> undefined | Value.
header_value(Hdr, Hdrs) ->
    header_value(Hdr, Hdrs, undefined).

%% @spec header_value(Header, Headers, Default) -> Default | term()
%% Header = string()
%% Headers = [{string(), term()}]
%% Value = term()
%% Default = term()
%% @doc
%% Returns the value associated with the `Header' in `Headers'.
%% `Header' must be a lowercase string, since every header is mangled to
%% check the match.  If no match is found, `Default' is returned.
%% @end
-spec header_value(string(), [{string(), Value}], Default) ->
    Default | Value.
header_value(Hdr, [{Hdr, Value} | _], _) ->
    Value;
header_value(Hdr, [{ThisHdr, Value}| Hdrs], Default) ->
    case string:equal(string:to_lower(ThisHdr), Hdr) of
        true  -> Value;
        false -> header_value(Hdr, Hdrs, Default)
    end;
header_value(_, [], Default) ->
    Default.

%% @spec (Item) -> OtherItem
%%   Item = atom() | list()
%%   OtherItem = list()
%% @doc
%% Will make any item, being an atom or a list, in to a list. If it is a
%% list, it is simple returned.
%% @end
-spec maybe_atom_to_list(atom() | list()) -> list().
maybe_atom_to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
maybe_atom_to_list(List) when is_list(List) ->
    List.

%% @spec (URL) -> {Host, Port, Path, Ssl}
%%   URL = string()
%%   Host = string()
%%   Port = integer()
%%   Path = string()
%%   Ssl = bool()
%% @doc
-spec parse_url(string()) -> {string(), integer(), string(), bool()}.
parse_url(URL) ->
    % XXX This should be possible to do with the re module?
    {Scheme, HostPortPath} = split_scheme(URL),
    {Host, PortPath} = split_host(HostPortPath, []),
    {Port, Path} = split_port(Scheme, PortPath, []),
    {string:to_lower(Host), Port, Path, Scheme =:= https}.

split_scheme("http://" ++ HostPortPath) ->
    {http, HostPortPath};
split_scheme("https://" ++ HostPortPath) ->
    {https, HostPortPath}.

split_host([$: | PortPath], Host) ->
    {lists:reverse(Host), PortPath};
split_host([$/ | _] = PortPath, Host) ->
    {lists:reverse(Host), PortPath};
split_host([H | T], Host) ->
    split_host(T, [H | Host]);
split_host([], Host) ->
    {lists:reverse(Host), []}.

split_port(http, [$/ | _] = Path, []) ->
    {80, Path};
split_port(https, [$/ | _] = Path, []) ->
    {443, Path};
split_port(http, [], []) ->
    {80, "/"};
split_port(https, [], []) ->
    {443, "/"};
split_port(_, [], Port) ->
    {list_to_integer(lists:reverse(Port)), "/"};
split_port(_,[$/ | _] = Path, Port) ->
    {list_to_integer(lists:reverse(Port)), Path};
split_port(Scheme, [P | T], Port) ->
    split_port(Scheme, T, [P | Port]).

%% @spec (Path, Method, Headers, Host, Body, PartialUpload) -> Request
%% Path = iolist()
%% Method = atom() | string()
%% Headers = [{atom() | string(), string()}]
%% Host = string()
%% Body = iolist()
%% PartialUpload = true | false
-spec format_request(iolist(), atom() | string(), headers(), string(),
    iolist(), true | false ) -> {true | false, iolist()}.
format_request(Path, Method, Hdrs, Host, Body, PartialUpload) ->
    AllHdrs = add_mandatory_hdrs(Method, Hdrs, Host, Body, PartialUpload),
    IsChunked = is_chunked(AllHdrs),
    {
        IsChunked,
        [
            Method, " ", Path, " HTTP/1.1\r\n",
            format_hdrs(AllHdrs),
            format_body(Body, IsChunked)
        ]
    }.

%% @spec normalize_method(AtomOrString) -> Method
%%   AtomOrString = atom() | string()
%%   Method = string()
%% @doc
%% Turns the method in to a string suitable for inclusion in a HTTP request
%% line.
%% @end
-spec normalize_method(atom() | string()) -> string().
normalize_method(Method) when is_atom(Method) ->
    string:to_upper(atom_to_list(Method));
normalize_method(Method) ->
    Method.

-spec format_hdrs(headers()) -> iolist().
format_hdrs(Headers) ->
    format_hdrs(Headers, []).

format_hdrs([{Hdr, Value} | T], Acc) ->
    NewAcc = [
        maybe_atom_to_list(Hdr), ":", maybe_atom_to_list(Value), "\r\n" | Acc
    ],
    format_hdrs(T, NewAcc);
format_hdrs([], Acc) ->
    [Acc, "\r\n"].

format_body(Body, false) ->
    Body;
format_body(Body, true) ->
    case iolist_size(Body) of
        0 ->
            <<>>;
        Size ->
            [
                erlang:integer_to_list(Size, 16), <<"\r\n">>,
                Body, <<"\r\n">>
            ]
    end.

add_mandatory_hdrs(Method, Hdrs, Host, Body, PartialUpload) ->
    add_host(add_content_headers(Method, Hdrs, Body, PartialUpload), Host).

add_content_headers("POST", Hdrs, Body, PartialUpload) ->
    add_content_headers(Hdrs, Body, PartialUpload);
add_content_headers("PUT", Hdrs, Body, PartialUpload) ->
    add_content_headers(Hdrs, Body, PartialUpload);
add_content_headers(_, Hdrs, _, _PartialUpload) ->
    Hdrs.

add_content_headers(Hdrs, Body, false) ->
    case header_value("content-length", Hdrs) of
        undefined ->
            ContentLength = integer_to_list(iolist_size(Body)),
            [{"Content-Length", ContentLength} | Hdrs];
        _ -> % We have a content length
            Hdrs
    end;
add_content_headers(Hdrs, _Body, true) ->
    case {header_value("content-length", Hdrs), 
         header_value("transfer-encoding", Hdrs)} of
        {undefined, undefined} ->
            [{"Transfer-Encoding", "chunked"} | Hdrs];
        {undefined, TransferEncoding} ->
            case string:to_lower(TransferEncoding) of
            "chunked" -> Hdrs;
            _ -> erlang:error({error, unsupported_transfer_encoding})
            end;
        {_Length, undefined} ->
            Hdrs;
        {_Length, _TransferEncoding} -> %% have both cont.length and chunked 
            erlang:error({error, bad_header})
    end.



add_host(Hdrs, Host) ->
    case header_value("host", Hdrs) of
        undefined ->
            [{"Host", Host } | Hdrs];
        _ -> % We have a host
            Hdrs
    end.

is_chunked(Hdrs) ->
    TransferEncoding = string:to_lower(
        header_value("transfer-encoding", Hdrs, "undefined")),
    case TransferEncoding of
        "chunked" -> true;
        _ -> false
    end.

-spec dec(timeout()) -> timeout().
dec(Num) when is_integer(Num) -> Num - 1;
dec(Else)                     -> Else.

