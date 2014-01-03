#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t
%%
%% This file is part of couchbeam released under the MIT license.
%% See the NOTICE for more information.

-include_lib("kernel/include/file.hrl").

main(_) ->
    etap:plan(37),
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
    Server = couchbeam:server_connection(),
    catch couchbeam:delete_db(Server, "couchbeam_testdb"),
    catch couchbeam:delete_db(Server, "couchbeam_testdb2"),
    ok.

stop_test() ->
    Server = couchbeam:server_connection(),
    catch couchbeam:delete_db(Server, "couchbeam_testdb"),
    catch couchbeam:delete_db(Server, "couchbeam_testdb2"),
    ok.


collect_mp({doc, Doc, Next}, Acc) ->
    collect_mp(couchbeam:stream_doc(Next), [{doc, Doc} | Acc]);
collect_mp({att, Name, Next}, Acc) ->
    collect_mp(couchbeam:stream_doc(Next), [{Name, <<>>} | Acc]);
collect_mp({att_body, Name, Chunk, Next}, Acc) ->
    Buffer = proplists:get_value(Name, Acc),
    NBuffer = << Buffer/binary, Chunk/binary >>,
    Acc1 = lists:keystore(Name, 1, Acc, {Name, NBuffer}),
    collect_mp(couchbeam:stream_doc(Next), Acc1);
collect_mp({att_eof, Name, Next}, Acc) ->
    collect_mp(couchbeam:stream_doc(Next), Acc);
collect_mp(eof, Acc) ->
    Acc.

test() ->
    Server = couchbeam:server_connection(),

    {ok, Db} = couchbeam:create_db(Server, "couchbeam_testdb"),
    Doc = {[
        {<<"_id">>, <<"test">>}
    ]},
    {ok, Doc1} = couchbeam:save_doc(Db, Doc),
    RevDoc1 = couchbeam_doc:get_value(<<"_rev">>, Doc1),
    io:format("rev ~p~n", [RevDoc1]),
    {ok, {Res}} = couchbeam:put_attachment(Db,"test", "test", "test",
            [{rev, RevDoc1}]),

    RevDoc11 = proplists:get_value(<<"rev">>, Res),
    etap:is(RevDoc1 =:= RevDoc11, false, "put attachment ok"),


    {ok, Attachment} = couchbeam:fetch_attachment(Db, "test", "test"),
    etap:is(Attachment, <<"test">>, "fetch attachment ok"),

    {ok, Doc2} = couchbeam:open_doc(Db, "test"),

    etap:is(case couchbeam:delete_attachment(Db, Doc2, "test") of
        {ok,  {_}} -> true;
        _ -> false
    end, true, "delete attachment ok"),

    Doc3 = {[
        {<<"_id">>, <<"test2">>}
    ]},
    Doc4 = couchbeam_attachments:add_inline(Doc3, "test", "test.txt"),
    Doc5 = couchbeam_attachments:add_inline(Doc4, "test2", "test2.txt"),
    {ok, _} = couchbeam:save_doc(Db, Doc5),
    {ok, Attachment1} = couchbeam:fetch_attachment(Db, "test2", "test.txt"),
    {ok, Attachment2} = couchbeam:fetch_attachment(Db, "test2", "test2.txt"),
    etap:is(Attachment1, <<"test">>, "fetch attachment ok"),
    etap:is(Attachment2, <<"test2">>, "fetch attachment ok"),

    {ok, Doc6} = couchbeam:open_doc(Db, "test2"),
    Doc7 = couchbeam_attachments:delete_inline(Doc6, "test2.txt"),
    {ok, _} = couchbeam:save_doc(Db, Doc7),
    etap:is(couchbeam:fetch_attachment(Db, "test2", "test2.txt"),
            {error, not_found}, "inline attachment deleted"),

    {ok, Attachment4} = couchbeam:fetch_attachment(Db, "test2", "test.txt"),
    etap:is(Attachment4, <<"test">>, "fetch attachment ok"),

    {ok, Doc8} = couchbeam:save_doc(Db, {[]}),

    {ok, FileInfo} = file:read_file_info("t/1M"),
    FileSize = FileInfo#file_info.size,
    {ok, Fd} = file:open("t/1M", [read]),
    {ok, _Res2} = couchbeam:put_attachment(Db, couchbeam_doc:get_id(Doc8),
        "1M", fun() ->
            case file:read(Fd, 4096) of
                {ok, Data} ->  {ok, iolist_to_binary(Data)};
                _ -> eof
            end
        end, [{content_length, FileSize}, {rev, couchbeam_doc:get_rev(Doc8)}]),
    file:close(Fd),

    {ok, Doc9} = couchbeam:open_doc(Db, couchbeam_doc:get_id(Doc8)),
    Attachements = couchbeam_doc:get_value(<<"_attachments">>, Doc9),
    etap:isnt(Attachements, undefined, "attachment stream ok"),
    Attachment5 = couchbeam_doc:get_value(<<"1M">>, Attachements),
    etap:isnt(Attachment5, undefined, "attachment 1M uploaded ok"),
    etap:is(couchbeam_doc:get_value(<<"length">>, Attachment5), FileSize, "attachment 1M size ok"),

    {ok, Bin} = couchbeam:fetch_attachment(Db, couchbeam_doc:get_id(Doc8), "1M"),
    etap:is(iolist_size(Bin), FileSize, "fetch streammed attachment ok"),


    {ok, _Res3}= couchbeam:put_attachment(Db, "test/2", "test", "test"),
    {ok, Attachment10} = couchbeam:fetch_attachment(Db, "test/2", "test"),
    etap:is(Attachment10, <<"test">>, "fetch attachment with encoded id ok"),

    {ok, _Res4}= couchbeam:put_attachment(Db, "test3", "test", "test"),
    {ok, Attachment11} = couchbeam:fetch_attachment(Db, "test3", "test"),
    etap:is(Attachment11, <<"test">>, "fetch attachment with clength ok"),

    %% test multipart related
    Resp = couchbeam:open_doc(Db, <<"test3">>, [{attachments, true}]),
    etap:ok(case Resp of
            {ok, {multipart, _}} -> true;
            _ -> false
        end, "multipart response OK"),

    {ok, {multipart, Stream}} = Resp,
    Collected = collect_mp(couchbeam:stream_doc(Stream), []),

    etap:ok(proplists:is_defined(doc, Collected),
            "doc in multipart response OK"),
    MpDoc = proplists:get_value(doc, Collected),
    MpDocId = couchbeam_doc:get_id(MpDoc),
    etap:is(MpDocId, <<"test3">>, "docid in multipart response OK"),
    etap:ok(proplists:is_defined(<<"test">>, Collected),
            "attachment in multipart response OK"),
    etap:is(proplists:get_value(<<"test">>, Collected), <<"test">>,
            "attachment content in multipart in response OK"),


    %% test multipart mixed response
    Resp1 = couchbeam:open_doc(Db, <<"test3">>, [{open_revs, all},
                                                 {accept, <<"multipart/mixed">>}]),
    etap:ok(case Resp1 of
            {ok, {multipart, _}} -> true;
            _ -> false
        end, "mixed multipart response OK"),

    {ok, {multipart, Stream1}} = Resp1,
    Collected1 = collect_mp(couchbeam:stream_doc(Stream1), []),
    etap:ok(proplists:is_defined(doc, Collected1),
            "doc in mixed multipart response OK"),
    MpDoc1 = proplists:get_value(doc, Collected1),
    MpDocId1 = couchbeam_doc:get_id(MpDoc1),
    etap:is(MpDocId1, <<"test3">>, "docid in mixed multipart response OK"),
    etap:ok(proplists:is_defined(<<"test">>, Collected1),
            "attachment in mixed multipart response OK"),
    etap:is(proplists:get_value(<<"test">>, Collected1), <<"test">>,
            "attachment content in mixed multipart in response OK"),


    RespMpDoc = couchbeam:save_doc(Db, {[{<<"_id">>, <<"test4">>}]},
                                   [{<<"test.txt">>, <<"test">>}], []),
    etap:ok(case RespMpDoc of
        {ok, {Props}} ->
            case proplists:get_value(<<"_id">>, Props) of
                <<"test4">> -> true;
                _ -> false
            end;
        _ -> false
    end, "save multipart doc  with id ok"),

    {ok, MpAttachment1} = couchbeam:fetch_attachment(Db, <<"test4">>,
                                                     <<"test.txt">>),
    etap:is(MpAttachment1, <<"test">>,
            "fetch attachment with multipart  doc ok"),



    RespMpDoc1 = couchbeam:save_doc(Db, {[{<<"_id">>, <<"test5">>}]},
                                    [{<<"1M">>, {file, "t/1M"}}], []),

    etap:ok(case RespMpDoc1 of
        {ok, {Props1}} ->
            case proplists:get_value(<<"_id">>, Props1) of
                <<"test5">> -> true;
                _ -> false
            end;
        _ -> false
    end, "save multipart doc with file ok"),
    {ok, Doc10} = couchbeam:open_doc(Db, <<"test5">>),
    MpAttachments = couchbeam_doc:get_value(<<"_attachments">>, Doc10),
    etap:isnt(MpAttachments, undefined, "file attachment with multipart
             OK"),
    MpAttachment2 = couchbeam_doc:get_value(<<"1M">>, MpAttachments),
    etap:isnt(MpAttachment2, undefined,
              "attachment 1M uploaded with mp dov OK"),
    etap:is(couchbeam_doc:get_value(<<"length">>, MpAttachment2), FileSize,
            "attachment 1M size ok"),

    {ok, MpBin} = couchbeam:fetch_attachment(Db, <<"test5">>, <<"1M">>),
    etap:is(iolist_size(MpBin), FileSize,
            "fetch streammed attachment from mp doc OK"),

    RespMpDoc2 = couchbeam:save_doc(Db, Doc10,
                                    [{<<"hello.txt">>, <<"world">>}], []),
    etap:ok(case RespMpDoc2 of
        {ok, {Props2}} ->
            case proplists:get_value(<<"_id">>, Props2) of
                <<"test5">> -> true;
                _ -> false
            end;
        _ -> false
    end, "save multipart doc with new att ok"),

    {ok, Doc11} = RespMpDoc2,
    MpAttachments1 = couchbeam_doc:get_value(<<"_attachments">>, Doc11),
    etap:isnt(MpAttachments1, undefined, "new attachment with multipart OK"),

    MpAttachment3 = couchbeam_doc:get_value(<<"1M">>, MpAttachments1),
    etap:isnt(MpAttachment3, undefined, "attachment 1M found"),
    etap:is(couchbeam_doc:get_value(<<"length">>, MpAttachment3), FileSize,
            "attachment 1M size ok"),

    {ok, MpBin1} = couchbeam:fetch_attachment(Db, <<"test5">>, <<"1M">>),
    etap:is(iolist_size(MpBin1), FileSize,
            "fetch streammed attachment from mp doc OK"),

    MpAttachment4 =  couchbeam_doc:get_value(<<"hello.txt">>, MpAttachments1),
    etap:isnt(MpAttachment4, undefined, "new attachment hello.txt found"),
    {ok, MpBin2} = couchbeam:fetch_attachment(Db, <<"test5">>,
                                              <<"hello.txt">>),
    etap:is(MpBin2, <<"world">>, "new attachment hello.txt OK"),
    ok.
