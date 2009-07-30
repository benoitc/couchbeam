#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(8),
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
    application:start(crypto),
    application:start(couchbeam),
    ok.

test() ->
    Data = couchbeam:server_info(default),
    etap:is(proplists:get_value(<<"couchdb">>, Data), <<"Welcome">>, "message ok"),
    F = fun() -> couchbeam:server_info(test) end,
    etap_exception:throws_ok(F, {unknown_couchdb_node,<<"No couchdb node configured for test.">>}, "error node ok"),
    etap:is(couchbeam:open_connection({test, {"127.0.0.1", 5984}}), ok, "open connection 1"),
    Data1 = couchbeam:server_info(test),
    etap:is(proplists:get_value(<<"couchdb">>, Data1), <<"Welcome">>, "message on new connection ok"),
    etap:is(couchbeam:open_connection({test2, {"127.0.0.1", 5984}}), ok, "open connection 2"),
    Data2 = couchbeam:server_info(test2),
    etap:is(proplists:get_value(<<"couchdb">>, Data2), <<"Welcome">>, "message on new connection ok"),
    
    %% try to override default
    etap:is(couchbeam:open_connection({default, {"127.0.0.1", 5984}}), ok, "override default ok"),
    
    %% in case node server is down, we try to recreate it from state.
    supervisor:terminate_child(couchbeam_nodes, test),
    supervisor:delete_child(couchbeam_nodes, test),
    etap:is(proplists:get_value(<<"couchdb">>, couchbeam:server_info(test)), 
        <<"Welcome">>, "test recreate node service"),
    ok.
