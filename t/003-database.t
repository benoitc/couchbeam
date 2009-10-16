#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(13),
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
    couchbeam_server:start_connection_link(),
    catch couchbeam_server:delete_db(default, "couchbeam_testdb"),
    catch couchbeam_server:delete_db(default, "couchbeam_testdb2"),
    catch couchbeam_server:delete_db(default, "couchbeam_testdb3"),
    ok.
    
stop_test() ->
    catch couchbeam_server:delete_db(default, "couchbeam_testdb"),
    catch couchbeam_server:delete_db(default, "couchbeam_testdb2"),
    catch couchbeam_server:delete_db(default, "couchbeam_testdb3"),
    ok.
    
test() ->
    Db = couchbeam_server:create_db(default, "couchbeam_testdb"),
    etap:is(is_pid(Db), true, "db created ok"),
    Db0 = couchbeam_server:create_db(default, "couchbeam_testdb"),
    etap:is(Db0, precondition_failed, "conclict database ok"),
    Db2 = couchbeam_server:create_db(default, "couchbeam_testdb2"),
    etap:is(is_pid(Db2), true, "db2 created ok"),
    AllDbs = couchbeam_server:all_dbs(default),
    etap:ok(is_list(AllDbs), "all_dbs return a list"),
    etap:ok(lists:member(<<"couchbeam_testdb">>, AllDbs), "couchbeam_testdb exists ok "),
    etap:ok(couchbeam_server:is_db(default, "couchbeam_testdb"), "is_db exists ok "),
    etap:ok(lists:member(<<"couchbeam_testdb2">>, AllDbs), "couchbeam_testdb2 exists ok"),
    etap:is(couchbeam_server:delete_db(default, "couchbeam_testdb2"), ok, "delete couchbeam_testdb2 ok"),
    AllDbs1 = couchbeam_server:all_dbs(default),
    etap:not_ok(lists:member(<<"couchbeam_testdb2">>, AllDbs1), "couchbeam_testdb2 don't exists ok"),
    couchbeam_db:close(default, Db),
    % test managed db
    Db3 = couchbeam_server:create_db(default, {testdb2, "couchbeam_testdb2"}),
    etap:is(is_pid(Db3), true, "db3 created ok"),
    etap:is(couchbeam_manager:get_db(testdb2), Db3, "db registered ok"),
    
    couchbeam_db:open_or_create(default, {testdb3, "couchbeam_testdb3"}),
    etap:ok(is_pid(couchbeam_manager:get_db(testdb3)), "db3 created and  registered ok"),
    AllDbs2 = couchbeam_server:all_dbs(default),
    etap:ok(lists:member(<<"couchbeam_testdb3">>, AllDbs2), "couchbeam_testdb3 exists ok"),
    ok.