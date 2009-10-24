#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin


-include_lib("kernel/include/file.hrl").

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
    ok.
    
stop_test() ->
    catch couchbeam_server:delete_db(default, "couchbeam_testdb"),
    catch couchbeam_server:delete_db(default, "couchbeam_testdb2"),
    ok.
    

get_streamed_attachment({ok, {http_eob, _Trailers}}, _Pid, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
get_streamed_attachment({ok, Bin}, Pid, Acc) ->
    NextState = couchbeam_resource:get_body_part(Pid),
    get_streamed_attachment(NextState, Pid, [binary_to_list(Bin)|Acc]).

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
    
    Doc8 = couchbeam_db:save_doc(Db, {[]}),
    {ok, FileInfo} = file:read_file_info("deps/lhttpc/test/1M"),
    FileSize = FileInfo#file_info.size,
    {ok, Fd} = file:open("deps/lhttpc/test/1M", [read]),
    Doc801 = couchbeam_db:put_attachment(Db, Doc8, fun() ->
        case file:read(Fd, 4096) of
            {ok, Data} ->  {ok, iolist_to_binary(Data)};
            _ -> eof
        end
    end, "1M", FileSize),
    file:close(Fd),
    Doc9 = couchbeam_db:open_doc(Db, couchbeam_doc:get_id(Doc801)),
    Attachements = couchbeam_doc:get_value(<<"_attachments">>, Doc9),
    etap:isnt(Attachements, undefined, "attachment stream ok"),
    Attachment5 = couchbeam_doc:get_value(<<"1M">>, Attachements),
    etap:isnt(Attachment5, undefined, "attachment 1M uploaded ok"),
    etap:is(couchbeam_doc:get_value(<<"length">>, Attachment5), FileSize, "attachment 1M size ok"),
    
    Pid = couchbeam_db:fetch_attachment(Db, couchbeam_doc:get_id(Doc801), "1M", true),
    etap:ok(is_pid(Pid), "get attachment pid ok"),
    InitialState = couchbeam_resource:get_body_part(Pid),
    Bin = get_streamed_attachment(InitialState, Pid, []),
    etap:is(iolist_size(Bin), FileSize, "fetch streammed attachment ok"),
    ok.