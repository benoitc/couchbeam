%%% -*- erlang -*-
%%%
%%% Test mocking helpers for couchbeam tests
%%% Provides utilities to mock HTTP layer for unit testing without CouchDB

-module(couchbeam_mocks).

-export([setup/0, teardown/0]).
-export([expect_response/3, expect_db_response/3]).
-export([set_body/2, get_body/1]).

%% @doc Initialize meck for hackney and couchbeam_httpc
setup() ->
    %% Unload any existing mocks first
    catch meck:unload(hackney),
    catch meck:unload(couchbeam_httpc),

    %% Create mocks with passthrough for unmocked functions
    meck:new(hackney, [passthrough, no_link]),
    meck:new(couchbeam_httpc, [passthrough, no_link]),

    %% Setup default mock for hackney:body to read from process dict
    meck:expect(hackney, body, fun(Ref) ->
        case get_body(Ref) of
            undefined -> {error, not_found};
            Body -> {ok, Body}
        end
    end),

    %% Setup default mock for hackney:skip_body
    meck:expect(hackney, skip_body, fun(_Ref) -> ok end),

    ok.

%% @doc Unload all mocks
teardown() ->
    catch meck:unload(hackney),
    catch meck:unload(couchbeam_httpc),
    ok.

%% @doc Set up an expectation for hackney:request
%% Matcher: fun(Method, Url, Headers, Body, Opts) -> boolean()
%% Response: {ok, Status, Headers, Ref} | {ok, Status, Headers} | {error, Reason}
expect_response(Matcher, Response, JsonBody) when is_function(Matcher, 5) ->
    meck:expect(hackney, request, fun(Method, Url, Headers, Body, Opts) ->
        case Matcher(Method, Url, Headers, Body, Opts) of
            true ->
                case Response of
                    {ok, Status, RespHeaders, Ref} ->
                        set_body(Ref, JsonBody),
                        {ok, Status, RespHeaders, Ref};
                    {ok, Status, RespHeaders} ->
                        {ok, Status, RespHeaders};
                    Other ->
                        Other
                end;
            false ->
                meck:passthrough([Method, Url, Headers, Body, Opts])
        end
    end).

%% @doc Set up an expectation for couchbeam_httpc:db_request
%% Matcher: fun(Method, Url, Headers, Body, Opts) -> boolean()
%% Response: {ok, Status, Headers, Ref} | {ok, Status, Headers} | {error, Reason}
expect_db_response(Matcher, Response, JsonBody) when is_function(Matcher, 5) ->
    meck:expect(couchbeam_httpc, db_request,
        fun(Method, Url, Headers, Body, Opts, _Expect) ->
            case Matcher(Method, Url, Headers, Body, Opts) of
                true ->
                    case Response of
                        {ok, Status, RespHeaders, Ref} ->
                            set_body(Ref, JsonBody),
                            {ok, Status, RespHeaders, Ref};
                        {ok, Status, RespHeaders} ->
                            {ok, Status, RespHeaders};
                        Other ->
                            Other
                    end;
                false ->
                    meck:passthrough([Method, Url, Headers, Body, Opts, _Expect])
            end
        end).

%% @doc Store a response body for a given ref
set_body(Ref, Body) when is_binary(Body) ->
    put({mock_body, Ref}, Body);
set_body(Ref, Term) ->
    put({mock_body, Ref}, couchbeam_ejson:encode(Term)).

%% @doc Retrieve and erase a stored response body
get_body(Ref) ->
    case erase({mock_body, Ref}) of
        undefined -> undefined;
        Body -> Body
    end.
