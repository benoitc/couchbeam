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
%%
%% @author Bob Ippolito <bob@mochimedia.com>
%% copyright 2007 Mochi Media, Inc.
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.


-module(couchbeam_util).

-export([generate_uuids/1, new_uuid/0, to_hex/1, to_digit/1, 
         join/2, revjoin/3, quote_plus/1, split/2, guess_mime/1, val/1]).


-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
                    (C >= $a andalso C =< $f) orelse
                    (C >= $A andalso C =< $F))).
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).
                      
hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

%% @spec quote_plus(atom() | integer() | float() | string() | binary()) -> string()
%% @doc URL safe encoding of the given term.
quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(Binary) when is_binary(Binary) ->
    quote_plus(binary_to_list(Binary));
quote_plus(Float) when is_float(Float) ->
    quote_plus(mochinum:digits(Float));
quote_plus(String) ->
    quote_plus(String, []).

quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]). 

generate_uuids(Count) ->
    [ new_uuid() || _ <- lists:seq(1,Count)].

%% Code from Mochiweb http://code.google.com/p/mochiweb/
%% @spec join([string()], Separator) -> string()
%% @doc Join a list of strings together with the given separator
%%   string or char.
join([], _Separator) ->
    [];
join([S], _Separator) ->
    lists:flatten(S);
join(Strings, Separator) ->
    lists:flatten(revjoin(lists:reverse(Strings), Separator, [])).
    
revjoin([], _Separator, Acc) ->
    Acc;
revjoin([S | Rest], Separator, []) ->
    revjoin(Rest, Separator, [S]);
revjoin([S | Rest], Separator, Acc) ->
    revjoin(Rest, Separator, [S, Separator | Acc]).
    

    
%% code from CouchDB
new_uuid() ->
    list_to_binary(to_hex(crypto:rand_bytes(16))).

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.

%% @spec split(Bin::binary(), Chars::string()) -> binary()
%% @doc split a binary
split(Bin, Chars) ->
    split(Chars, Bin, 0, []).

split(Chars, Bin, Idx, Acc) ->
    case Bin of
        <<This:Idx/binary, Char, Tail/binary>> ->
                case lists:member(Char, Chars) of
                        false ->
                                split(Chars, Bin, Idx+1, Acc);
                        true ->
                                split(Chars, Tail, 0, [This|Acc])
                end;
        <<This:Idx/binary>> ->
                lists:reverse(Acc, [This])
    end.


val(V) when is_list(V) ->
    V;    
val(V) when is_integer(V) ->
    integer_to_list(V);
val(V) when is_binary(V) ->
    binary_to_list(V).

%% @spec guess_mime(string()) -> string()
%% @doc  Guess the mime type of a file by the extension of its filename.
guess_mime(File) ->
    case filename:extension(File) of
        ".html" ->
            "text/html";
        ".xhtml" ->
            "application/xhtml+xml";
        ".xml" ->
            "application/xml";
        ".css" ->
            "text/css";
        ".js" ->
            "application/x-javascript";
        ".jpg" ->
            "image/jpeg";
        ".gif" ->
            "image/gif";
        ".png" ->
            "image/png";
        ".swf" ->
            "application/x-shockwave-flash";
        ".zip" ->
            "application/zip";
        ".bz2" ->
            "application/x-bzip2";
        ".gz" ->
            "application/x-gzip";
        ".tar" ->
            "application/x-tar";
        ".tgz" ->
            "application/x-gzip";
        ".txt" ->
            "text/plain";
        ".doc" ->
            "application/msword";
        ".pdf" ->
            "application/pdf";
        ".xls" ->
            "application/vnd.ms-excel";
        ".rtf" ->
            "application/rtf";
        ".mov" ->
            "video/quicktime";
        ".mp3" ->
            "audio/mpeg";
        ".z" ->
            "application/x-compress";
        ".wav" ->
            "audio/x-wav";
        ".ico" ->
            "image/x-icon";
        ".bmp" ->
            "image/bmp";
        ".m4a" ->
            "audio/mpeg";
        ".m3u" ->
            "audio/x-mpegurl";
        ".exe" ->
            "application/octet-stream";
        ".csv" ->
            "text/csv";
        _ ->
            "text/plain"
    end.