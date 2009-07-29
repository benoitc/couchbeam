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
    application:start(ecouchdbkit),
    catch ecouchdbkit:delete_db(default, "ecouchdbkit_testdb"),
    catch ecouchdbkit:delete_db(default, "ecouchdbkit_testdb2"),
    ok.
    
test() ->
    etap:is(ecouchdbkit:create_db(default, "ecouchdbkit_testdb"), ok, "db created ok"),
    etap:ok(case ecouchdbkit:save_doc(default, "ecouchdbkit_testdb", {[{"test", blah}]}) of
        {ok, _} -> true;
        _E -> false
        end, "save doc ok"),
    etap:ok(case ecouchdbkit:save_doc(default, "ecouchdbkit_testdb", 
            {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]}) of
        {ok, {[{<<"id">>,<<"test">>}|_]}} -> true;
        _ -> false
        end, "save do with id ok"),
    F = fun() -> 
        ecouchdbkit:save_doc(default, "ecouchdbkit_testdb", 
            {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]})
    end,
    etap_exception:throws_ok(F, conflict, "conflict raised"),
    {Doc} = ecouchdbkit:open_doc(default, "ecouchdbkit_testdb", "test"),
    etap:is(proplists:get_value(<<"test">>, Doc), <<"blah">>, "fetch doc ok"),
    ecouchdbkit:save_doc(default, "ecouchdbkit_testdb", 
        {[{<<"_id">>,<<"test2">>}, {<<"test">>,<<"blah">>}]}),
    F1 = fun() ->
        ecouchdbkit:open_doc(default, "ecouchdbkit_testdb", "test2")
    end,
    Doc1 = F1(),
    etap_exception:lives_ok(F1,"test2 has been created"),
    etap:ok(case ecouchdbkit:delete_doc(default,"ecouchdbkit_testdb", Doc1) of
        {ok, _} -> true;
        _E2 -> false
        end, "doc2 has been deleted"),
    etap_exception:throws_ok(F1, not_found, "test2 not found"),
    ok.
    