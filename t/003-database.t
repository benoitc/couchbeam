#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t

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
    Conn = couchbeam_server:start_connection_link(),
    catch couchbeam_server:delete_db(Conn, "couchbeam_testdb"),
    catch couchbeam_server:delete_db(Conn, "couchbeam_testdb2"),
    catch couchbeam_server:delete_db(Conn, "couchbeam_testdb3"),
    ok.
    
stop_test() ->
    Conn = couchbeam_server:start_connection_link(),
    catch couchbeam_server:delete_db(Conn, "couchbeam_testdb"),
    catch couchbeam_server:delete_db(Conn, "couchbeam_testdb2"),
    catch couchbeam_server:delete_db(Conn, "couchbeam_testdb3"),
    ok.
    
test() ->
    Conn = couchbeam_server:start_connection_link(),
    Db = couchbeam_server:create_db(Conn, "couchbeam_testdb"),
    etap:is(is_pid(Db), true, "db created ok"),
    Db0 = couchbeam_server:create_db(Conn, "couchbeam_testdb"),
    etap:is(Db0, db_exists, "database already loaded ok"),
    Db2 = couchbeam_server:create_db(Conn, "couchbeam_testdb2"),
    etap:is(is_pid(Db2), true, "db2 created ok"),
    AllDbs = couchbeam_server:all_dbs(Conn),
    etap:ok(is_list(AllDbs), "all_dbs return a list"),
    etap:ok(lists:member(<<"couchbeam_testdb">>, AllDbs), "couchbeam_testdb exists ok "),
    etap:ok(couchbeam_server:is_db(Conn, "couchbeam_testdb"), "is_db exists ok "),
    etap:ok(lists:member(<<"couchbeam_testdb2">>, AllDbs), "couchbeam_testdb2 exists ok"),
    etap:is(couchbeam_server:delete_db(Conn, "couchbeam_testdb2"), ok, "delete couchbeam_testdb2 ok"),
    AllDbs1 = couchbeam_server:all_dbs(Conn),
    etap:not_ok(lists:member(<<"couchbeam_testdb2">>, AllDbs1), "couchbeam_testdb2 don't exists ok"),
    couchbeam_db:close(Db),
    ok.
