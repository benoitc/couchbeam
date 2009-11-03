#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(12),
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
    DesignDoc = {[
        {<<"_id">>, <<"_design/couchbeam">>},
        {<<"language">>,<<"javascript">>},
        {<<"views">>,
            {[{<<"test">>,
                {[{<<"map">>,
                    <<"function (doc) {\n if (doc.type == \"test\") {\n emit(doc._id, doc);\n}\n}">>
                }]}
            }]}
        }]},
    Doc = {[
        {<<"type">>, <<"test">>}
    ]},
    couchbeam_db:save_doc(Db, DesignDoc),
    couchbeam_db:save_doc(Db, Doc),
    couchbeam_db:save_doc(Db, Doc),
    AllDocs = couchbeam_db:all_docs(Db, []),
    {T, _, _, R} = couchbeam_view:parse_view(AllDocs),
    etap:is(length(R), 3, "all_docs: nb doc is ok"),
    etap:is(T, 3, "all_docs: TotalsRow ok"),
    AllDocs1 = couchbeam_db:all_docs(Db, [{"limit", "1"}]),
    {_, _, _, R1} = couchbeam_view:parse_view(AllDocs1),
    etap:is(length(R1), 1, "nb doc is ok"),
    Doc1 = {[
        {<<"_id">>, <<"test">>},
        {<<"type">>, <<"test">>}
    ]},
    Doc2 = {[
        {<<"_id">>, <<"test2">>},
        {<<"type">>, <<"test">>}
    ]},
    Doc3 = {[
        {<<"_id">>, <<"test3">>},
        {<<"type">>, <<"test">>}
    ]},
    couchbeam_db:save_doc(Db, Doc1),
    couchbeam_db:save_doc(Db, Doc2),
    couchbeam_db:save_doc(Db, Doc3),
    VResults = couchbeam_db:query_view(Db, {"couchbeam", "test"}, []),
    {T2, _, _, R2} = couchbeam_view:parse_view(VResults),
    etap:is(T2, 5, "view: total_rows in view ok"),
    etap:is(length(R2), 5, "view: nb rows ok"),
    etap:is(couchbeam_view:count(VResults), 5, "view count: nb rows ok"),
    VResults2 = couchbeam_db:query_view(Db, {"couchbeam", "test"}, [{"key", <<"test">>}]),
    etap:is(couchbeam_view:count(VResults2), 1, "view, key : nb rows ok"),
    VResults3 = couchbeam_db:query_view(Db, {"couchbeam", "test"}, [{"startkey", <<"test">>}]),
    etap:is(couchbeam_view:count(VResults3), 3, "view, startkey, nb rows ok"),
    VRresults4 = couchbeam_db:query_view(Db, {"couchbeam", "test"}, [{"startkey", <<"test">>}, {"endkey", <<"test2">>}]),
    etap:is(couchbeam_view:count(VRresults4), 2, "view, startkey, endkey : nb rows ok"),
    VResults5 = couchbeam_db:query_view(Db, {"couchbeam",  "test"}, [{"keys", [<<"test">>, <<"test3">>]}]),
    etap:is(couchbeam_view:count(VResults5), 2, "view, keys : nb rows ok"),
    {Id, _, _} = couchbeam_view:first(VResults5),
    etap:is(Id, <<"test">>, "first key ok"),
    ok.