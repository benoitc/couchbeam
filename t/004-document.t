#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(3),
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
            {[{<<"_id">>,<<"test">>}, {<<"test">>,"blah"}]}) of
        {ok, [{<<"id">>,<<"test">>}|_]} -> true;
        _ -> false
        end, "save do with id ok"),
    
    ok.