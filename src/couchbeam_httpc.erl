%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_httpc).

-export([request/5,
         db_request/5, db_request/6,
         json_body/1,
         db_resp/2,
         make_headers/4,
         maybe_oauth_header/4]).

request(Method, Url, Headers, Body, Options) ->
    {FinalHeaders, FinalOpts} = make_headers(Method, Url, Headers,
                                             Options),
    hackney:request(Method, Url , FinalHeaders, Body, FinalOpts).

db_request(Method, Url, Headers, Body, Options) ->
    db_request(Method, Url, Headers, Body, Options, []).

db_request(Method, Url, Headers, Body, Options, Expect) ->
    Resp = request(Method, Url, Headers, Body, Options),
    db_resp(Resp, Expect).

json_body(Ref) ->
    {ok, Body} = hackney:body(Ref),
    couchbeam_ejson:decode(Body).

make_headers(Method, Url, Headers, Options) ->
    Headers1 = [{<<"Accept">>, <<"application/json, */*;q=0.9">>} | Headers],
    maybe_oauth_header(Method, Url, Headers1, Options).

maybe_oauth_header(Method, Url, Headers, Options) ->
    case couchbeam_util:get_value(oauth, Options) of
        undefined ->
            {Headers, Options};
        OauthProps ->
            Hdr = couchbeam_util:oauth_header(Url, Method, OauthProps),
            {[Hdr|Headers], proplists:delete(oauth, Options)}
    end.

db_resp({ok, Ref}=Resp, _Expect) when is_reference(Ref) ->
    Resp;
db_resp({ok, 404, _, Ref}, _Expect) ->
    hackney:skip_body(Ref),
    {error, not_found};
db_resp({ok, 409, _, Ref}, _Expect) ->
    hackney:skip_body(Ref),
    {error, conflict};
db_resp({ok, 412, _, Ref}, _Expect) ->
    hackney:skip_body(Ref),
    {error, precondition_failed};
db_resp({ok, _, _, _}=Resp, []) ->
    Resp;
db_resp({ok, Status, Headers, Ref}=Resp, Expect) ->
    case lists:member(Status, Expect) of
        true -> Resp;
        false ->
            {ok, Body} = hackney:body(Ref),
            {error, {bad_response, {Status, Headers, Body}}}
    end;
db_resp(Error, _Expect) ->
    Error.
