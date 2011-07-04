%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license. 
%%% See the NOTICE for more information.

-module(couchbeam_util).

-export([encode_docid/1, encode_att_name/1]).
-export([parse_options/1, parse_options/2]).
-export([to_list/1, to_binary/1, to_integer/1, to_atom/1]).
-export([encode_query/1, encode_query_value/2]).
-export([oauth_header/3]).
-export([propmerge/3, propmerge1/2]).
-export([get_value/2, get_value/3]).
-export([guess_mime/1, quote_plus/1, urlencode/1]).
-export([parse_qs/1, urlsplit/1]).
-export([deprecated/3]).

-define(PERCENT, 37).  % $\%
-define(ENCODE_DOCID, true).
-define(FULLSTOP, 46). % $\.
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
                    (C >= $a andalso C =< $f) orelse
                    (C >= $A andalso C =< $F))).
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

encode_att_name(Name) when is_binary(Name) ->
    encode_att_name(xmerl_ucs:from_utf8(Name));
encode_att_name(Name) ->
    Parts = lists:foldl(fun(P, Att) ->
               [xmerl_ucs:to_utf8(P)|Att]
       end, [], string:tokens(Name, "/")),
    lists:flatten(Parts).

encode_docid(DocId) when is_binary(DocId) ->
    encode_docid(binary_to_list(DocId));
encode_docid(DocId)->
    case ?ENCODE_DOCID of
        true -> encode_docid1(DocId);
        false -> DocId
    end.
    
encode_docid1(DocId) ->
    case DocId of
        "_design/" ++ Rest ->
            Rest1 = encode_docid(Rest),
            "_design/" ++ Rest1;
        _ ->
            ibrowse_lib:url_encode(DocId)
    end.

%% @doc Encode needed value of Query proplists in json
encode_query([]) ->
    [];
encode_query(QSL) when is_list(QSL) ->
    lists:foldl(fun({K, V}, Acc) ->
        V1 = encode_query_value(K, V), 
        [{K, V1}|Acc]
    end, [], QSL);
encode_query(QSL) ->
    QSL.

%% @doc Encode value in JSON if needed depending on the key 
encode_query_value(K, V) when is_atom(K) ->
    encode_query_value(atom_to_list(K), V);
encode_query_value(K, V) when is_binary(K) ->
    encode_query_value(binary_to_list(K), V);
encode_query_value(K, V) ->
    case K of
        "key" -> encode_value(V);
        "startkey" -> encode_value(V);
        "endkey" -> encode_value(V);
        _ -> V
    end.

encode_value(V) ->
    V1 = ejson:encode(V),
    binary_to_list(iolist_to_binary(V1)).

% build oauth header
oauth_header(Url, Action, OauthProps) ->
    {_, _, _, QS, _} = urlsplit(Url),
    QSL = parse_qs(QS),

    % get oauth paramerers
    ConsumerKey = to_list(get_value(consumer_key, OauthProps)),
    Token = to_list(get_value(token, OauthProps)),
    TokenSecret = to_list(get_value(token_secret, OauthProps)),
    ConsumerSecret = to_list(get_value(consumer_secret, OauthProps)),
    SignatureMethodStr = to_list(get_value(signature_method, 
            OauthProps, "HMAC-SHA1")),

    SignatureMethodAtom = case SignatureMethodStr of
        "PLAINTEXT" ->
            plaintext;
        "HMAC-SHA1" ->
            hmac_sha1;
        "RSA-SHA1" ->
            rsa_sha1
    end,
    Consumer = {ConsumerKey, ConsumerSecret, SignatureMethodAtom},
    Method = case Action of
        delete -> "DELETE";
        get -> "GET";
        post -> "POST";
        put -> "PUT";
        head -> "HEAD"
    end,
    Params = oauth:signed_params(Method, Url, QSL, Consumer, Token, TokenSecret)
    -- QSL,
    {"Authorization", "OAuth " ++ oauth_uri:params_to_header_string(Params)}.


%% @doc merge 2 proplists. All the Key - Value pairs from both proplists
%% are included in the new proplists. If a key occurs in both dictionaries 
%% then Fun is called with the key and both values to return a new
%% value. This a wreapper around dict:merge
propmerge(F, L1, L2) ->
	dict:to_list(dict:merge(F, dict:from_list(L1), dict:from_list(L2))).

%% @doc Update a proplist with values of the second. In case the same
%% key is in 2 proplists, the value from the first are kept.
propmerge1(L1, L2) ->
    propmerge(fun(_, V1, _) -> V1 end, L1, L2).


%% @doc emulate proplists:get_value/2,3 but use faster lists:keyfind/3
-spec(get_value/2 :: (Key :: term(), Prop :: [term()] ) -> term()).
get_value(Key, Prop) ->
    get_value(Key, Prop, undefined).

-spec(get_value/3 :: (Key :: term(), Prop :: [term()], Default :: term() ) -> term()).
get_value(Key, Prop, Default) ->
    case lists:keyfind(Key, 1, Prop) of
	false ->
	    case lists:member(Key, Prop) of
		true -> true;
		false -> Default
	    end;
	{Key, V} -> % only return V if a two-tuple is found
	    V;
	Other when is_tuple(Other) -> % otherwise return the default
	    Default
    end.    

%% @doc make view options a list
parse_options(Options) ->
    parse_options(Options, []).

parse_options([], Acc) ->
    Acc;
parse_options([V|Rest], Acc) when is_atom(V) ->
    parse_options(Rest, [{atom_to_list(V), true}|Acc]);
parse_options([{K,V}|Rest], Acc) when is_list(K) ->    
    parse_options(Rest, [{K,V}|Acc]);
parse_options([{K,V}|Rest], Acc) when is_binary(K) ->
    parse_options(Rest, [{binary_to_list(K),V}|Acc]);
parse_options([{K,V}|Rest], Acc) when is_atom(K) ->   
    parse_options(Rest, [{atom_to_list(K),V}|Acc]);
parse_options(_,_) ->
    fail.

to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_list(V) ->
    try
        list_to_binary(V)
    catch
        _ ->
            list_to_binary(io_lib:format("~p", [V]))
    end;
to_binary(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
to_binary(V) ->
    V.

to_integer(V) when is_integer(V) ->
    V;
to_integer(V) when is_list(V) ->
    erlang:list_to_integer(V);
to_integer(V) when is_binary(V) ->
    erlang:list_to_integer(binary_to_list(V)).

to_list(V) when is_list(V) ->
    V;
to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_atom(V) ->
    atom_to_list(V);
to_list(V) ->
    V.

to_atom(V) when is_atom(V) ->
    V;
to_atom(V) when is_list(V) ->
    list_to_atom(V);
to_atom(V) when is_binary(V) ->
    list_to_atom(binary_to_list(V));
to_atom(V) ->
    list_to_atom(lists:flatten(io_lib:format("~p", [V]))).


%% @spec guess_mime(string()) -> string()
%% @doc  Guess the mime type of a file by the extension of its filename.
guess_mime(File) ->
    case couchbeam_mime:from_extension(filename:extension(File)) of
        undefined ->
            "text/plain";
        Mime ->
            Mime
    end.


hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.

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

%% @spec urlencode([{Key, Value}]) -> string()
%% @doc URL encode the property list.
urlencode(Props) ->
    Pairs = lists:foldr(
              fun ({K, V}, Acc) ->
                      [quote_plus(K) ++ "=" ++ quote_plus(V) | Acc]
              end, [], Props),
    string:join(Pairs, "&").


%% @spec parse_qs(string() | binary()) -> [{Key, Value}]
%% @doc Parse a query string or application/x-www-form-urlencoded.
parse_qs(Binary) when is_binary(Binary) ->
    parse_qs(binary_to_list(Binary));
parse_qs(String) ->
    parse_qs(String, []).

parse_qs([], Acc) ->
    lists:reverse(Acc);
parse_qs(String, Acc) ->
    {Key, Rest} = parse_qs_key(String),
    {Value, Rest1} = parse_qs_value(Rest),
    parse_qs(Rest1, [{Key, Value} | Acc]).

parse_qs_key(String) ->
    parse_qs_key(String, []).

parse_qs_key([], Acc) ->
    {qs_revdecode(Acc), ""};
parse_qs_key([$= | Rest], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_key(Rest=[$; | _], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_key(Rest=[$& | _], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_key([C | Rest], Acc) ->
    parse_qs_key(Rest, [C | Acc]).

parse_qs_value(String) ->
    parse_qs_value(String, []).

parse_qs_value([], Acc) ->
    {qs_revdecode(Acc), ""};
parse_qs_value([$; | Rest], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_value([$& | Rest], Acc) ->
    {qs_revdecode(Acc), Rest};
parse_qs_value([C | Rest], Acc) ->
    parse_qs_value(Rest, [C | Acc]).



qs_revdecode(S) ->
    qs_revdecode(S, []).

qs_revdecode([], Acc) ->
    Acc;
qs_revdecode([$+ | Rest], Acc) ->
    qs_revdecode(Rest, [$\s | Acc]);
qs_revdecode([Lo, Hi, ?PERCENT | Rest], Acc) when ?IS_HEX(Lo), ?IS_HEX(Hi) ->
    qs_revdecode(Rest, [(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)) | Acc]);
qs_revdecode([C | Rest], Acc) ->
    qs_revdecode(Rest, [C | Acc]).

%% @spec urlsplit(Url) -> {Scheme, Netloc, Path, Query, Fragment}
%% @doc Return a 5-tuple, does not expand % escapes. Only supports HTTP style
%%      URLs.
urlsplit(Url) ->
    {Scheme, Url1} = urlsplit_scheme(Url),
    {Netloc, Url2} = urlsplit_netloc(Url1),
    {Path, Query, Fragment} = urlsplit_path(Url2),
    {Scheme, Netloc, Path, Query, Fragment}.

urlsplit_scheme(Url) ->
    case urlsplit_scheme(Url, []) of
        no_scheme ->
            {"", Url};
        Res ->
            Res
    end.

urlsplit_scheme([C | Rest], Acc) when ((C >= $a andalso C =< $z) orelse
                                       (C >= $A andalso C =< $Z) orelse
                                       (C >= $0 andalso C =< $9) orelse
                                       C =:= $+ orelse C =:= $- orelse
                                       C =:= $.) ->
    urlsplit_scheme(Rest, [C | Acc]);
urlsplit_scheme([$: | Rest], Acc=[_ | _]) ->
    {string:to_lower(lists:reverse(Acc)), Rest};
urlsplit_scheme(_Rest, _Acc) ->
    no_scheme.

urlsplit_netloc("//" ++ Rest) ->
    urlsplit_netloc(Rest, []);
urlsplit_netloc(Path) ->
    {"", Path}.

urlsplit_netloc("", Acc) ->
    {lists:reverse(Acc), ""};
urlsplit_netloc(Rest=[C | _], Acc) when C =:= $/; C =:= $?; C =:= $# ->
    {lists:reverse(Acc), Rest};
urlsplit_netloc([C | Rest], Acc) ->
    urlsplit_netloc(Rest, [C | Acc]).

%% @spec urlsplit_path(Url) -> {Path, Query, Fragment}
%% @doc Return a 3-tuple, does not expand % escapes. Only supports HTTP style
%%      paths.
urlsplit_path(Path) ->
    urlsplit_path(Path, []).

urlsplit_path("", Acc) ->
    {lists:reverse(Acc), "", ""};
urlsplit_path("?" ++ Rest, Acc) ->
    {Query, Fragment} = urlsplit_query(Rest),
    {lists:reverse(Acc), Query, Fragment};
urlsplit_path("#" ++ Rest, Acc) ->
    {lists:reverse(Acc), "", Rest};
urlsplit_path([C | Rest], Acc) ->
    urlsplit_path(Rest, [C | Acc]).

urlsplit_query(Query) ->
    urlsplit_query(Query, []).

urlsplit_query("", Acc) ->
    {lists:reverse(Acc), ""};
urlsplit_query("#" ++ Rest, Acc) ->
    {lists:reverse(Acc), Rest};
urlsplit_query([C | Rest], Acc) ->
    urlsplit_query(Rest, [C | Acc]).

deprecated(Old, New, When) ->
    io:format(
      <<
        "WARNING: function deprecated~n"
        "Function '~p' has been deprecated~n"
        "in favor of '~p'.~n"
        "'~p' will be removed ~s.~n~n"
      >>, [Old, New, Old, When]).
