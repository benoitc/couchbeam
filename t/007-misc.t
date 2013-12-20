#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -pa ./t
%%
%% This file is part of couchbeam released under the MIT license.
%% See the NOTICE for more information.



main(_) ->
    etap:plan(3),
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
    catch couchbeam:delete_db(Server, "couchbeam_testdb3"),
    ok.

stop_test() ->
    Server = couchbeam:server_connection(),
    catch couchbeam:delete_db(Server, "couchbeam_testdb"),
    catch couchbeam:delete_db(Server, "couchbeam_testdb2"),
    catch couchbeam:delete_db(Server, "couchbeam_testdb3"),
    ok.

test() ->
    Server = couchbeam:server_connection(),
    {ok, Db} = couchbeam:create_db(Server, "couchbeam_testdb"),
    {ok, Db2} = couchbeam:create_db(Server, "couchbeam_testdb2"),

    %% test replication

    {ok, Doc11} = couchbeam:save_doc(Db, {[]}),
    DocId11 = couchbeam_doc:get_id(Doc11),
    DocRev11 = couchbeam_doc:get_rev(Doc11),
    etap:ok(case couchbeam:replicate(Server, Db, Db2) of
        {ok, _} -> true;
        _ -> false
    end, "replicate OK"),
    {ok, Doc11_2} = couchbeam:open_doc(Db2, DocId11),
    DocRev11_2 = couchbeam_doc:get_rev(Doc11_2),
    etap:is(DocRev11_2, DocRev11, "replicate doc OK"),

    %% test missing revs

    {ok, Doc12} = couchbeam:save_doc(Db, Doc11 ),
    {ok, Doc13} = couchbeam:save_doc(Db, Doc12),

    DocId12 = couchbeam_doc:get_id(Doc12),
    DocRev12 = couchbeam_doc:get_rev(Doc12),
    DocId13 = couchbeam_doc:get_id(Doc13),
    DocRev13 = couchbeam_doc:get_rev(Doc13),
    {ok, Missing} = couchbeam:get_missing_revs(Db2, [{DocId11, [DocRev12,
                                                                DocRev13]}]),
    etap:is(Missing, [{DocId11, [DocRev12, DocRev13], [DocRev11]}],
        "get missing revs OK"),

    ok.


