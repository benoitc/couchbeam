#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(18),
    clean_tests(),
    case (catch test()) of
        ok ->
            clean_tests(),
            etap:end_tests();
        Other ->
            clean_tests(),
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail()
    end,
    ok.

clean_tests() ->
    Node = {"127.0.0.1", 5984},
    catch couchbeam:delete_db(Node, "couchbeam_testdb"),
    catch couchbeam:delete_db(Node, "couchbeam_testdb2"),
    ok.
    
    
    
test() ->
    Node = {"127.0.0.1", 5984},
    Data = couchbeam:server_info(Node),
    etap:is(proplists:get_value(<<"couchdb">>, Data), <<"Welcome">>, "message ok"),
    F = fun() -> couchbeam:server_info({"127.0.0.1", 10000}) end,
    etap_exception:throws_ok(F, {bad_request,econnrefused}, "error node ok"),
    etap:is(couchbeam:create_db(Node, "couchbeam_testdb"), ok, "db created ok"),
    F1 = fun() -> couchbeam:create_db(Node, "couchbeam_testdb") end,
    etap_exception:throws_ok(F1, precondition_failed, "conclict database ok"),
    etap:is(couchbeam:create_db(Node, "couchbeam_testdb2"), ok, "db2 created ok"),
    AllDbs = couchbeam:all_dbs(Node),
    etap:ok(is_list(AllDbs), "all_dbs return a list"),
    etap:ok(lists:member(<<"couchbeam_testdb">>, AllDbs), "couchbeam_testdb exists ok "),
    etap:ok(couchbeam:is_db(Node, "couchbeam_testdb"), "is_db exists ok "),
    etap:ok(lists:member(<<"couchbeam_testdb2">>, AllDbs), "couchbeam_testdb2 exists ok"),
    etap:is(couchbeam:delete_db(Node, "couchbeam_testdb2"), ok, "delete couchbeam_testdb2 ok"),
    AllDbs1 = couchbeam:all_dbs(Node),
    etap:not_ok(lists:member(<<"couchbeam_testdb2">>, AllDbs1), "couchbeam_testdb2 don't exists ok"),
    etap:ok(case couchbeam:save_doc(Node, "couchbeam_testdb", {[{"test", blah}]}) of
        {ok, _} -> true;
        _E -> false
        end, "save doc ok"),
    etap:ok(case couchbeam:save_doc(Node, "couchbeam_testdb", 
            {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]}) of
        {ok, {[{<<"id">>,<<"test">>}|_]}} -> true;
        _ -> false
        end, "save do with id ok"),
    F2 = fun() -> 
        couchbeam:save_doc(Node, "couchbeam_testdb", 
            {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]})
    end,
    etap_exception:throws_ok(F2, conflict, "conflict raised"),
    {Doc} = couchbeam:open_doc(Node, "couchbeam_testdb", "test"),
    etap:is(proplists:get_value(<<"test">>, Doc), <<"blah">>, "fetch doc ok"),
    couchbeam:save_doc(Node, "couchbeam_testdb", 
        {[{<<"_id">>,<<"test2">>}, {<<"test">>,<<"blah">>}]}),
    F3 = fun() ->
        couchbeam:open_doc(Node, "couchbeam_testdb", "test2")
    end,
    Doc1 = F3(),
    etap_exception:lives_ok(F3,"test2 has been created"),
    etap:ok(case couchbeam:delete_doc(Node,"couchbeam_testdb", Doc1) of
        {ok, _} -> true;
        _E2 -> false
        end, "doc2 has been deleted"),
    etap_exception:throws_ok(F3, not_found, "test2 not found"),
    ok.