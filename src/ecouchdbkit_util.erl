%% @author Benoît Chesneau <benoitc@e-engura.org>
%% @copyright 2009 Benoît Chesneau.
%%
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.
%%
%%


-module(ecouchdbkit_util).

-export([generate_uuids/1, new_uuid/0, to_hex/1, to_digit/1, 
         join/2, revjoin/3, url_encode/1]).

generate_uuids(Count) ->
    [ new_uuid() || _ <- lists:seq(1,Count)].


%% Code from Mochiweb http://code.google.com/p/mochiweb/
%% @spec join([string()], Separator) -> string()
%% @doc Join a list of strings together with the given separator
%%      string or char.
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
    

%% @doc URL-encodes a string based on RFC 1738. Returns a flat list.
%% http://erlyaws.svn.sourceforge.net/viewvc/erlyaws/trunk/yaws/src/yaws_api.erl
%% @spec url_encode(Str) -> UrlEncodedStr
%% Str = string()
%% UrlEncodedStr = string()  

url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        true ->
            case erlang:list_to_integer([H], 16) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].
    
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

