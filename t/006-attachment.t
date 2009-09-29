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
    Doc = {[
        {<<"_id">>, <<"test">>}
    ]},
    Doc1 = couchbeam_db:save_doc(Db, Doc),
    RevDoc1 = couchbeam_doc:get_value(<<"_rev">>, Doc1),
    Doc11 = couchbeam_db:put_attachment(Db, Doc1, "test", "test", length("test")),
    RevDoc11 = couchbeam_doc:get_value(<<"_rev">>, Doc11),
    etap:is(RevDoc1 =:= RevDoc11, false, "put attachment ok"),
    Attachment = couchbeam_db:fetch_attachment(Db, "test", "test"),
    etap:is(Attachment, <<"test">>, "fetch attachment ok"),
    Doc2 = couchbeam_db:open_doc(Db, "test"),
    RevDoc2 = couchbeam_doc:get_value(<<"_rev">>, Doc2),
    Doc21 = couchbeam_db:delete_attachment(Db, Doc2, "test"),
    RevDoc21 = couchbeam_doc:get_value(<<"_rev">>, Doc21),
    etap:is(RevDoc2 =:= RevDoc21, false, "delete attachment ok"),
    Doc3 = {[
        {<<"_id">>, <<"test2">>}
    ]},
    Doc4 = couchbeam_doc:add_attachment(Doc3, "test", "test.txt"),
    Doc5 = couchbeam_doc:add_attachment(Doc4, "test2", "test2.txt"),
    couchbeam_db:save_doc(Db, Doc5),
    Attachment1 = couchbeam_db:fetch_attachment(Db, "test2", "test.txt"),
    Attachment2 = couchbeam_db:fetch_attachment(Db, "test2", "test2.txt"),
    etap:is(Attachment1, <<"test">>, "fetch attachment ok"),
    etap:is(Attachment2, <<"test2">>, "fetch attachment ok"),
    Doc6 = couchbeam_db:open_doc(Db, "test2"),
    Doc7 = couchbeam_doc:delete_inline_attachment(Doc6, "test2.txt"),
    couchbeam_db:save_doc(Db, Doc7),
    Attachment3 = couchbeam_db:fetch_attachment(Db, "test2", "test2.txt"),
    etap:is(Attachment3, not_found, "inline attachment deleted"),
    Attachment4 = couchbeam_db:fetch_attachment(Db, "test2", "test.txt"),
    etap:is(Attachment4, <<"test">>, "fetch attachment ok"),
    ok.