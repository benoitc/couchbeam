#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(8),
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
    application:start(crypto),
    application:start(couchbeam),
    catch couchbeam:delete_db(default, "couchbeam_testdb"),
    catch couchbeam:delete_db(default, "couchbeam_testdb2"),
    ok.
    
stop_test() ->
    catch couchbeam:delete_db(default, "couchbeam_testdb"),
    catch couchbeam:delete_db(default, "couchbeam_testdb2"),
    ok.
    
test() ->
    etap:is(couchbeam:create_db(default, "couchbeam_testdb"), ok, "db created ok"),
    etap:ok(case couchbeam:save_doc(default, "couchbeam_testdb", {[{"test", blah}]}) of
        {ok, _} -> true;
        _E -> false
        end, "save doc ok"),
    etap:ok(case couchbeam:save_doc(default, "couchbeam_testdb", 
            {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]}) of
        {ok, {[{<<"id">>,<<"test">>}|_]}} -> true;
        _ -> false
        end, "save do with id ok"),
    F = fun() -> 
        couchbeam:save_doc(default, "couchbeam_testdb", 
            {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]})
    end,
    etap_exception:throws_ok(F, conflict, "conflict raised"),
    {Doc} = couchbeam:open_doc(default, "couchbeam_testdb", "test"),
    etap:is(proplists:get_value(<<"test">>, Doc), <<"blah">>, "fetch doc ok"),
    couchbeam:save_doc(default, "couchbeam_testdb", 
        {[{<<"_id">>,<<"test2">>}, {<<"test">>,<<"blah">>}]}),
    F1 = fun() ->
        couchbeam:open_doc(default, "couchbeam_testdb", "test2")
    end,
    Doc1 = F1(),
    etap_exception:lives_ok(F1,"test2 has been created"),
    etap:ok(case couchbeam:delete_doc(default,"couchbeam_testdb", Doc1) of
        {ok, _} -> true;
        _E2 -> false
        end, "doc2 has been deleted"),
    etap_exception:throws_ok(F1, not_found, "test2 not found"),
    ok.
    