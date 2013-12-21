#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t
%%
%% This file is part of couchbeam released under the MIT license.
%% See the NOTICE for more information.


main(_) ->
    etap:plan(9),
    start_app(),
    case (catch test()) of
        ok ->
            stop_test(),
            etap:end_tests();
        Other ->
            stop_test(),
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

start_app() ->
    couchbeam:start().

stop_test() ->
    ok.

test() ->
    Server = couchbeam:server_connection(),
    {ok, {AllConfig}} = couchbeam:get_config(Server),
    etap:ok(proplists:is_defined(<<"httpd">>, AllConfig), "all config OK"),

    {ok, {HttpdConfig}} = couchbeam:get_config(Server, <<"httpd">>),
    etap:ok(proplists:is_defined(<<"port">>, HttpdConfig),
            "section config OK"),

    {ok, Port} = couchbeam:get_config(Server, <<"httpd">>, <<"port">>),
    etap:is(Port, <<"5984">>, "section, key config OK"),

    etap:is(couchbeam:set_config(Server, <<"couchbeam">>,
                                 <<"test">>, <<"1">>, false),
            {ok, <<>>},
            "set config OK"),

    etap:is(couchbeam:get_config(Server, <<"couchbeam">>,<<"test">>),
            {ok, <<"1">>},
            "check set OK"),

    etap:is(couchbeam:set_config(Server, <<"couchbeam">>,
                                 <<"test">>, <<"2">>,  false),
            {ok, <<"1">>},
            "update config OK"),

    etap:is(couchbeam:get_config(Server, <<"couchbeam">>, <<"test">>),
            {ok, <<"2">>},
            "check update OK"),

    etap:is(couchbeam:delete_config(Server, <<"couchbeam">>,
                                    <<"test">>, false),
            {ok, <<"2">>},
            "delete config OK"),

    etap:is(couchbeam:get_config(Server, <<"couchbeam">>,  <<"test">>),
            {error, not_found},
            "check delete OK"),
    ok.
