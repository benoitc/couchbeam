#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(10),
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
    DesignDoc = {[
        {<<"_id">>, <<"_design/test">>},
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
    ecouchdbkit:save_doc(default, "ecouchdbkit_testdb", DesignDoc),
    ecouchdbkit:save_doc(default, "ecouchdbkit_testdb", Doc),
    ecouchdbkit:save_doc(default, "ecouchdbkit_testdb", Doc),
    AllDocs = ecouchdbkit:all_docs(default, "ecouchdbkit_testdb", []),
    {T, O, R} = ecouchdbkit:parse_view(AllDocs),
    etap:is(length(R), 3, "all_docs: nb doc is ok"),
    etap:is(T, 3, "all_docs: TotalsRow ok"),
    AllDocs1 = ecouchdbkit:all_docs(default, "ecouchdbkit_testdb", [{"limit", "1"}]),
    {_, _, R1} = ecouchdbkit:parse_view(AllDocs1),
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
    ecouchdbkit:save_doc(default, "ecouchdbkit_testdb", Doc1),
    ecouchdbkit:save_doc(default, "ecouchdbkit_testdb", Doc2),
    ecouchdbkit:save_doc(default, "ecouchdbkit_testdb", Doc3),
    VResults = ecouchdbkit:query_view(default, "ecouchdbkit_testdb", "test", "test"),
    {T2, _, R2} = ecouchdbkit:parse_view(VResults),
    etap:is(T2, 5, "view: total_rows in view ok"),
    etap:is(length(R2), 5, "view: nb rows ok"),
    VResults2 = ecouchdbkit:query_view(default, "ecouchdbkit_testdb", "test", 
                                "test", [{"key", <<"test">>}]),
    {_,_, R3} = ecouchdbkit:parse_view(VResults2),
    etap:is(length(R3), 1, "view, key : nb rows ok"),
    VResults3 = ecouchdbkit:query_view(default, "ecouchdbkit_testdb", "test", 
                                "test", [{"startkey", <<"test">>}]),
    {_,_, R4} = ecouchdbkit:parse_view(VResults3),
    etap:is(length(R4), 3, "view, startkey, nb rows ok"),
    VRresults4 = ecouchdbkit:query_view(default, "ecouchdbkit_testdb", "test", 
                    "test", [{"startkey", <<"test">>}, {"endkey", <<"test2">>}]),
    {_,_, R5} = ecouchdbkit:parse_view(VRresults4),
    etap:is(length(R5), 2, "view, startkey, endkey : nb rows ok"),
    VResults5 = ecouchdbkit:query_view(default, "ecouchdbkit_testdb", "test", 
                    "test", [{"keys", [<<"test">>, <<"test3">>]}]),
    {_,_, R6} = ecouchdbkit:parse_view(VResults5),
    etap:is(length(R6), 2, "view, keys : nb rows ok"),
    ok.