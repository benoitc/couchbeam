%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_util).

-include_lib("hackney/include/hackney.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

-export([dbname/1]).
-export([encode_docid/1, encode_att_name/1]).
-export([parse_options/1, parse_options/2]).
-export([to_list/1, to_binary/1, to_integer/1, to_atom/1]).
-export([encode_query/1, encode_query_value/2]).
-export([oauth_header/3]).
-export([propmerge/3, propmerge1/2]).
-export([get_value/2, get_value/3]).
-export([deprecated/3, shutdown_sync/1]).
-export([start_app_deps/1, get_app_env/2]).
-export([encode_docid1/1, encode_docid_noop/1]).
-export([force_param/3]).
-export([proxy_token/2, proxy_header/3]).

-define(PROXY_AUTH_HEADERS,[
    {username,<<"X-Auth-CouchDB-UserName">>},
    {roles,<<"X-Auth-CouchDB-Roles">>},
    {token,<<"X-Auth-CouchDB-Token">>}]).

-define(ENCODE_DOCID_FUNC, encode_docid1).

dbname(DbName) when is_list(DbName) ->
    list_to_binary(DbName);
dbname(DbName) when is_binary(DbName) ->
    DbName;
dbname(DbName) ->
    erlang:error({illegal_database_name, DbName}).

encode_att_name(Name) when is_binary(Name) ->
    encode_att_name(xmerl_ucs:from_utf8(Name));
encode_att_name(Name) ->
    Parts = lists:foldl(fun(P, Att) ->
               [xmerl_ucs:to_utf8(P)|Att]
       end, [], string:tokens(Name, "/")),
    lists:flatten(Parts).

encode_docid(DocId) when is_list(DocId) ->
    encode_docid(list_to_binary(DocId));
encode_docid(DocId)->
    ?ENCODE_DOCID_FUNC(DocId).

encode_docid1(DocId) ->
    case DocId of
        << "_design/", Rest/binary >> ->
            Rest1 = hackney_url:urlencode(Rest, [noplus]),
            <<"_design/", Rest1/binary >>;
        _ ->
            hackney_url:urlencode(DocId, [noplus])
    end.

encode_docid_noop(DocId) ->
    DocId.

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
encode_query_value(_K, V) -> V.

% build oauth header
oauth_header(Url, Action, OauthProps) when is_binary(Url) ->
    oauth_header(binary_to_list(Url),Action, OauthProps);
oauth_header(Url, Action, OauthProps) ->
    #hackney_url{qs=QS} = hackney_url:parse_url(Url),
    QSL = [{binary_to_list(K), binary_to_list(V)} || {K,V} <-
                                                     hackney_url:parse_qs(QS)],

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
    Params = oauth:sign(Method, Url, QSL, Consumer, Token, TokenSecret) -- QSL,

    Realm = "OAuth " ++ oauth:header_params_encode(Params),
    {<<"Authorization">>, list_to_binary(Realm)}.


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

%% @doc replace a value in a proplist
force_param(Key, Value, Options) ->
    case couchbeam_util:get_value(Key, Options) of
        undefined ->
            [{Key, Value} | Options];
        _ ->
            lists:keystore(Key, 1, Options, {Key, Value})
    end.

%% @doc emulate proplists:get_value/2,3 but use faster lists:keyfind/3
-spec get_value(Key :: term(), Prop :: [term()]) -> term().
get_value(Key, Prop) ->
    get_value(Key, Prop, undefined).

-spec get_value(Key :: term(), Prop :: [term()], Default :: term()) -> term().
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

deprecated(Old, New, When) ->
    io:format(
      <<
        "WARNING: function deprecated~n"
        "Function '~p' has been deprecated~n"
        "in favor of '~p'.~n"
        "'~p' will be removed ~s.~n~n"
      >>, [Old, New, Old, When]).

shutdown_sync(Pid) when not is_pid(Pid)->
    ok;
shutdown_sync(Pid) ->
    MRef = erlang:monitor(process, Pid),
    try
        catch unlink(Pid),
        catch exit(Pid, shutdown),
        receive
        {'DOWN', MRef, _, _, _} ->
            ok
        end
    after
        erlang:demonitor(MRef, [flush])
    end.

%% @spec start_app_deps(App :: atom()) -> ok
%% @doc Start depedent applications of App.
start_app_deps(App) ->
    {ok, DepApps} = application:get_key(App, applications),
    [ensure_started(A) || A <- DepApps],
    ok.

%% @spec ensure_started(Application :: atom()) -> ok
%% @doc Start the named application if not already started.
ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

get_app_env(Env, Default) ->
    case application:get_env(couchbeam, Env) of
        {ok, Val} -> Val;
        undefined -> Default
    end.

proxy_header(UserName,Roles,Secret) ->
    proxy_header(UserName,Roles,Secret,?PROXY_AUTH_HEADERS).

proxy_header(UserName,Roles,Secret,HeaderNames) ->
    proxy_header_token(UserName,Roles,proxy_token(Secret,UserName),HeaderNames).

proxy_header_token(UserName,Roles,Token,L) ->
[
    {hgv(username,L), UserName},
    {hgv(roles,L), Roles},
    {hgv(token,L), Token}
].

hgv(N,L) ->
    get_value(N,L,get_value(N,?PROXY_AUTH_HEADERS)).

proxy_token(Secret,UserName) ->
    hackney_bstr:to_hex(hmac(sha, Secret, UserName)).

hmac(Alg, Key, Data) ->
    code:ensure_loaded(crypto),
    case {Alg, erlang:function_exported(crypto, hmac, 3)} of
        {_, true} ->
            crypto:hmac(Alg, Key, Data);
        {sha, false} ->
            crypto:sha_mac(Key, Data)
    end.
