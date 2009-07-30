#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(4),
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
    Doc = {[
        {<<"_id">>, <<"test">>}
    ]},
    {ok, Res} = couchbeam:save_doc(default, "couchbeam_testdb", Doc),
    Rev = couchbeam:get_value(<<"rev">>, Res),
    Doc1 = couchbeam:extend({<<"_rev">>, Rev}, Doc),
    { Ok, _} = couchbeam:put_attachment(default, "couchbeam_testdb", Doc1, 
        "test", "test", length("test")),
    etap:is(Ok, ok, "put attachment ok"),
    {raw, Attachment} = couchbeam:fetch_attachment(default, "couchbeam_testdb", "test", "test"),
    etap:is(Attachment, <<"test">>, "fetch attachment ok"),
    Doc2 = couchbeam:open_doc(default, "couchbeam_testdb", "test"),
    {Ok1, _} = couchbeam:delete_attachment(default, "couchbeam_testdb", Doc2, "test"),
    etap:is(Ok1, ok, "delete attachment ok"),
    
    ok.