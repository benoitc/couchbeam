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
    couchbeam:start(),
    couchbeam_server:start_connection_link(),
    catch couchbeam_server:delete_db(default, "couchbeam_testdb"),
    catch couchbeam_server:delete_db(default, "couchbeam_testdb2"),
    ok.
    
stop_test() ->
    catch couchbeam_server:delete_db(default, "couchbeam_testdb"),
    catch couchbeam_server:delete_db(default, "couchbeam_testdb2"),
    ok.
    
test() ->
    Db = couchbeam_server:create_db(default, "couchbeam_testdb"),
    etap:is(is_pid(Db), true, "db created ok"),
    Doc = couchbeam_db:save_doc(Db, {[{<<"test">>, <<"blah">>}]}),
    etap:ok(case Doc of
        {_} -> true;
        _ -> false
    end, "save doc ok"),
    etap:ok(case couchbeam_db:save_doc(Db, 
            {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]}) of
        {Props} -> 
            case proplists:get_value(<<"_id">>, Props) of
                <<"test">> -> true;
                _ -> false 
            end;
        _ -> false
    end, "save do with id ok"),
    Doc0 = couchbeam_db:save_doc(Db, 
            {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]}),
    etap:is(Doc0, conflict, "conflict raised"),
    {Doc1} = couchbeam_db:open_doc(Db, "test"),
    etap:is(proplists:get_value(<<"test">>, Doc1), <<"blah">>, "fetch doc ok"),
    couchbeam_db:save_doc(Db, 
        {[{<<"_id">>,<<"test2">>}, {<<"test">>,<<"blah">>}]}),
    
    Doc2 = couchbeam_db:open_doc(Db, "test2"),
    etap:ok(case Doc2 of
        {_} -> true;
        _ -> false
        end, "test2 has been created"),
    etap:ok(case couchbeam_db:delete_doc(Db, Doc2) of
        {_} -> true;
        _ -> false
        end, "doc2 has been deleted"),
    etap:is(couchbeam_db:open_doc(Db, "test2"), not_found, "test2 not found"),
    ok.
    