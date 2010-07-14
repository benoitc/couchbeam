#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t

main(_) ->
    etap:plan(1),
    start_app(),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.
    
start_app() ->
    couchbeam:start(),
    ok.

test() ->
    Connection = couchbeam_server:start_connection_link(),
    
    Data = couchbeam_server:info(Connection),
    etap:is(proplists:get_value(<<"couchdb">>, Data), <<"Welcome">>, "message ok"),
    ok.
