#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(27),
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
    
    % test managed db
    Db1 = couchbeam_server:create_db(default, {testdb2, "couchbeam_testdb2"}),
    Doc3 = couchbeam_db:save_doc(testdb2, {[{<<"test">>, <<"blah">>}]}),
    etap:ok(case Doc3 of
           {_} -> true;
           _ -> false
       end, "save doc in managed db ok"),
    
    Doc4 = {[{<<"a">>, 1}]},
    etap:is(couchbeam_doc:get_value(<<"a">>, Doc4), 1, "get value ok"),
    etap:is(couchbeam_doc:get_value("a", Doc4), 1, "get value from string ok"),
    etap:is(couchbeam_doc:get_value("b", Doc4), undefined, "get undefined value ok"),
    etap:is(couchbeam_doc:get_value("b", Doc4, nil), nil, "get undefined value with default ok"),
    Doc5 = couchbeam_doc:set_value("b", 1, Doc4),
    etap:is(couchbeam_doc:get_value("b", Doc5), 1, "set value ok"),
    Doc6 = couchbeam_doc:set_value("b", 0, Doc5),
    etap:is(couchbeam_doc:get_value("b", Doc6), 0, "update value ok"),
    Doc7 = couchbeam_doc:delete_value("b", Doc6),
    etap:is(couchbeam_doc:get_value("b", Doc7), undefined, "delete value ok"),
    Doc8 = couchbeam_doc:extend([{<<"b">>, 1}, {<<"c">>, 1}], Doc7),
    etap:is(couchbeam_doc:get_value("b", Doc8), 1, "set value ok"),
    etap:is(couchbeam_doc:get_value("c", Doc8), 1, "set value ok"),
    
    Doc9 = {[{<<"_id">>, <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>}]},
    Doc10 = couchbeam_db:save_doc(Db, Doc9),
    Doc101 = couchbeam_db:open_doc(Db, <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>),
    etap:ok(case Doc10 of
        {_} -> true;
        _ -> false
        end, "doc with special char created ok"),
    etap:is(couchbeam_doc:get_value(<<"_id">>, Doc101), <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>, "doc with special char created ok 2"),
    
    Doc11 = {[{<<"f">>, 1}]},
    etap:not_ok(couchbeam_doc:is_saved(Doc11), "document isn't saved ok"),
    etap:is(couchbeam_doc:get_id(Doc11), undefined, "document id is undefined ok"),
    etap:is(couchbeam_doc:get_rev(Doc11), undefined, "document rev is undefined ok"),
    Doc12 = couchbeam_db:save_doc(Db, Doc11),
    etap:ok(couchbeam_doc:is_saved(Doc12), "document saved ok"),
    etap:isnt(couchbeam_doc:get_id(Doc12), undefined, "document id  defined ok"),
    etap:isnt(couchbeam_doc:get_rev(Doc12), undefined, "document rev is defined ok"),
    
    Doc13 = couchbeam_db:save_doc(Db, {[]}),
    Doc14 = couchbeam_db:save_doc(Db, {[]}),
    couchbeam_db:delete_docs(Db, [Doc13, Doc14]),
    
    etap:is(couchbeam_db:open_doc(Db, couchbeam_doc:get_id(Doc13)), not_found, "bulk docs delete ok"),
    
    ok.
    