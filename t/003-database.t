#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

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
    couchbeam:start(),
    ok.
    
stop_test() ->
    catch couchapp_server:delete_db("couchbeam_testdb"),
    catch couchbeam:delete_db(default, "couchbeam_testdb2"),
    ok.
    
test() ->
    etap:is(couchbeam:create_db(default, "couchbeam_testdb"), ok, "db created ok"),
    F = fun() -> couchbeam:create_db(default, "couchbeam_testdb") end,
    etap_exception:throws_ok(F, precondition_failed, "conclict database ok"),
    etap:is(couchbeam:create_db(default, "couchbeam_testdb2"), ok, "db2 created ok"),
    AllDbs = couchbeam:all_dbs(default),
    etap:ok(is_list(AllDbs), "all_dbs return a list"),
    etap:ok(lists:member(<<"couchbeam_testdb">>, AllDbs), "couchbeam_testdb exists ok "),
    etap:ok(couchbeam:is_db(default, "couchbeam_testdb"), "is_db exists ok "),
    etap:ok(lists:member(<<"couchbeam_testdb2">>, AllDbs), "couchbeam_testdb2 exists ok"),
    etap:is(couchbeam:delete_db(default, "couchbeam_testdb2"), ok, "delete couchbeam_testdb2 ok"),
    AllDbs1 = couchbeam:all_dbs(default),
    etap:not_ok(lists:member(<<"couchbeam_testdb2">>, AllDbs1), "couchbeam_testdb2 don't exists ok"),
    ok.