#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(9),
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
    application:start(ecouchdbkit),
    catch ecouchdbkit:delete_db(default, "ecouchdbkit_testdb"),
    catch ecouchdbkit:delete_db(default, "ecouchdbkit_testdb2"),
    ok.
    
test() ->
    etap:is(ecouchdbkit:create_db(default, "ecouchdbkit_testdb"), ok, "db created ok"),
    F = fun() -> ecouchdbkit:create_db(default, "ecouchdbkit_testdb") end,
    etap_exception:throws_ok(F, precondition_failed, "conclict database ok"),
    etap:is(ecouchdbkit:create_db(default, "ecouchdbkit_testdb2"), ok, "db2 created ok"),
    AllDbs = ecouchdbkit:all_dbs(default),
    etap:ok(is_list(AllDbs), "all_dbs return a list"),
    etap:ok(lists:member(<<"ecouchdbkit_testdb">>, AllDbs), "ecouchdbkit_testdb exists ok "),
    etap:ok(ecouchdbkit:is_db(default, "ecouchdbkit_testdb"), "is_db exists ok "),
    etap:ok(lists:member(<<"ecouchdbkit_testdb2">>, AllDbs), "ecouchdbkit_testdb2 exists ok"),
    etap:is(ecouchdbkit:delete_db(default, "ecouchdbkit_testdb2"), ok, "delete ecouchdbkit_testdb2 ok"),
    AllDbs1 = ecouchdbkit:all_dbs(default),
    etap:not_ok(lists:member(<<"ecouchdbkit_testdb2">>, AllDbs1), "ecouchdbkit_testdb2 don't exists ok"),
    ok.