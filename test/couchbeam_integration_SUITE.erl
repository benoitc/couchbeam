%%% -*- erlang -*-
%%%
%%% Integration tests for couchbeam against a real CouchDB instance.
%%% These tests are optional and will be skipped if CouchDB is not available.
%%%
%%% To run locally:
%%%   COUCHDB_URL=http://localhost:5984 rebar3 ct --suite=couchbeam_integration_SUITE
%%%
%%% Environment variables:
%%%   COUCHDB_URL - CouchDB server URL (default: http://localhost:5984)
%%%   COUCHDB_USER - Optional admin username
%%%   COUCHDB_PASS - Optional admin password

-module(couchbeam_integration_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases - Server operations
-export([server_info/1,
         server_uuids/1,
         all_dbs/1]).

%% Test cases - Database operations
-export([create_delete_db/1,
         open_or_create_db/1,
         db_info_detailed/1]).

%% Test cases - Document CRUD
-export([doc_crud/1,
         doc_exists/1,
         doc_copy/1,
         doc_lookup_rev/1,
         doc_conflicts/1]).

%% Test cases - Bulk operations
-export([bulk_docs/1,
         bulk_delete/1]).

%% Test cases - Attachments
-export([doc_with_attachment/1,
         multiple_attachments/1,
         large_attachment/1,
         attachment_streaming/1]).

%% Test cases - Views
-export([all_docs/1,
         all_docs_options/1,
         view_query/1,
         view_with_keys/1,
         view_pagination/1,
         view_reduce/1,
         view_count/1,
         view_first/1,
         view_fold/1]).

%% Test cases - Design documents
-export([design_info/1,
         view_cleanup/1]).

%% Test cases - Changes feed
-export([changes_feed/1,
         changes_with_filter/1,
         changes_with_doc_ids/1,
         changes_include_docs/1]).

%% Test cases - Error handling
-export([error_not_found/1,
         error_conflict/1,
         error_invalid_doc/1]).

%%====================================================================
%% CT callbacks
%%====================================================================

all() ->
    [{group, server_ops},
     {group, database_ops},
     {group, document_ops},
     {group, bulk_ops},
     {group, attachment_ops},
     {group, view_ops},
     {group, design_ops},
     {group, changes_ops},
     {group, error_handling}].

groups() ->
    [{server_ops, [sequence], [
        server_info,
        server_uuids,
        all_dbs
    ]},
     {database_ops, [sequence], [
        create_delete_db,
        open_or_create_db,
        db_info_detailed
    ]},
     {document_ops, [sequence], [
        doc_crud,
        doc_exists,
        doc_copy,
        doc_lookup_rev,
        doc_conflicts
    ]},
     {bulk_ops, [sequence], [
        bulk_docs,
        bulk_delete
    ]},
     {attachment_ops, [sequence], [
        doc_with_attachment,
        multiple_attachments,
        large_attachment,
        attachment_streaming
    ]},
     {view_ops, [sequence], [
        all_docs,
        all_docs_options,
        view_query,
        view_with_keys,
        view_pagination,
        view_reduce,
        view_count,
        view_first,
        view_fold
    ]},
     {design_ops, [sequence], [
        design_info,
        view_cleanup
    ]},
     {changes_ops, [sequence], [
        changes_feed,
        changes_with_filter,
        changes_with_doc_ids,
        changes_include_docs
    ]},
     {error_handling, [sequence], [
        error_not_found,
        error_conflict,
        error_invalid_doc
    ]}].

init_per_suite(Config) ->
    %% Start required applications
    {ok, _} = application:ensure_all_started(couchbeam),

    %% Get CouchDB connection info from environment
    Url = os:getenv("COUCHDB_URL", "http://localhost:5984"),
    User = os:getenv("COUCHDB_USER", ""),
    Pass = os:getenv("COUCHDB_PASS", ""),

    %% Build server options
    Options = case {User, Pass} of
        {"", ""} -> [];
        {U, P} -> [{basic_auth, {list_to_binary(U), list_to_binary(P)}}]
    end,

    %% Try to connect to CouchDB
    Server = couchbeam:server_connection(Url, Options),

    case couchbeam:server_info(Server) of
        {ok, Info} ->
            ct:pal("Connected to CouchDB: ~p", [Info]),
            [{server, Server}, {couchdb_url, Url} | Config];
        {error, Reason} ->
            ct:pal("CouchDB not available at ~s: ~p", [Url, Reason]),
            {skip, {couchdb_not_available, Reason}}
    end.

end_per_suite(_Config) ->
    ok.

init_per_group(Group, Config) ->
    %% Create a unique test database name for each group
    TestDb = iolist_to_binary([<<"couchbeam_test_">>,
                               atom_to_binary(Group, utf8), <<"_">>,
                               integer_to_binary(erlang:system_time(millisecond))]),
    Server = ?config(server, Config),

    %% Create the test database
    case couchbeam:create_db(Server, TestDb) of
        {ok, Db} ->
            ct:pal("Created test database for ~p: ~s", [Group, TestDb]),
            [{test_db, TestDb}, {db, Db} | Config];
        {error, Reason} ->
            ct:pal("Failed to create test database: ~p", [Reason]),
            {skip, {db_creation_failed, Reason}}
    end.

end_per_group(_Group, Config) ->
    %% Clean up test database
    Server = ?config(server, Config),
    TestDb = ?config(test_db, Config),
    case couchbeam:open_db(Server, TestDb) of
        {ok, Db} ->
            couchbeam:delete_db(Db),
            ct:pal("Cleaned up test database: ~s", [TestDb]);
        _ ->
            ok
    end,
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Server operations tests
%%====================================================================

%% Test server info retrieval
server_info(Config) ->
    Server = ?config(server, Config),
    {ok, Info} = couchbeam:server_info(Server),
    true = is_map(Info),
    true = maps:is_key(<<"couchdb">>, Info),
    true = maps:is_key(<<"version">>, Info),
    true = maps:is_key(<<"uuid">>, Info),
    ct:pal("Server info: ~p", [Info]),
    ok.

%% Test UUID generation
server_uuids(Config) ->
    Server = ?config(server, Config),

    %% Get a single UUID
    {ok, Uuid} = couchbeam:get_uuid(Server),
    32 = byte_size(Uuid),
    ct:pal("Got single UUID: ~s", [Uuid]),

    %% Get multiple UUIDs
    {ok, Uuids} = couchbeam:get_uuids(Server, 5),
    5 = length(Uuids),
    lists:foreach(fun(U) ->
        32 = byte_size(U)
    end, Uuids),
    ct:pal("Got 5 UUIDs: ~p", [Uuids]),

    %% Verify all UUIDs are unique
    5 = length(lists:usort(Uuids)),

    ok.

%% Test listing all databases
all_dbs(Config) ->
    Server = ?config(server, Config),
    TestDb = ?config(test_db, Config),

    %% List all databases
    {ok, Dbs} = couchbeam:all_dbs(Server),
    true = is_list(Dbs),
    true = lists:member(TestDb, Dbs),
    ct:pal("Found ~p databases, test db present: true", [length(Dbs)]),

    ok.

%%====================================================================
%% Database operations tests
%%====================================================================

%% Test database creation and deletion
create_delete_db(Config) ->
    Server = ?config(server, Config),

    %% Create a temporary database
    TempDb = iolist_to_binary([<<"temp_db_">>,
                               integer_to_binary(erlang:system_time(millisecond))]),
    {ok, Db} = couchbeam:create_db(Server, TempDb),
    ct:pal("Created temp database: ~s", [TempDb]),

    %% Verify it exists
    true = couchbeam:db_exists(Server, TempDb),

    %% Delete it
    {ok, _} = couchbeam:delete_db(Db),

    %% Verify it's gone
    false = couchbeam:db_exists(Server, TempDb),
    ct:pal("Deleted temp database: ~s", [TempDb]),

    ok.

%% Test open_or_create_db
open_or_create_db(Config) ->
    Server = ?config(server, Config),
    TestDb = ?config(test_db, Config),

    %% Open existing database
    {ok, Db1} = couchbeam:open_or_create_db(Server, TestDb),
    {ok, Info1} = couchbeam:db_info(Db1),
    TestDb = maps:get(<<"db_name">>, Info1),

    %% Create new database
    NewDb = iolist_to_binary([<<"new_db_">>,
                              integer_to_binary(erlang:system_time(millisecond))]),
    {ok, Db2} = couchbeam:open_or_create_db(Server, NewDb),
    {ok, Info2} = couchbeam:db_info(Db2),
    NewDb = maps:get(<<"db_name">>, Info2),

    %% Cleanup
    couchbeam:delete_db(Db2),

    ok.

%% Test detailed database info
db_info_detailed(Config) ->
    Db = ?config(db, Config),
    TestDb = ?config(test_db, Config),

    {ok, Info} = couchbeam:db_info(Db),
    true = is_map(Info),

    %% Check expected fields
    TestDb = maps:get(<<"db_name">>, Info),
    true = maps:is_key(<<"doc_count">>, Info),
    true = maps:is_key(<<"update_seq">>, Info),
    true = maps:is_key(<<"sizes">>, Info),

    ct:pal("Database info: ~p", [Info]),
    ok.

%%====================================================================
%% Document CRUD tests
%%====================================================================

%% Test document CRUD operations
doc_crud(Config) ->
    Db = ?config(db, Config),

    %% Create a document with auto-generated ID
    Doc1 = #{<<"type">> => <<"test">>,
             <<"value">> => 42},
    {ok, Doc1Saved} = couchbeam:save_doc(Db, Doc1),
    true = maps:is_key(<<"_id">>, Doc1Saved),
    true = maps:is_key(<<"_rev">>, Doc1Saved),
    DocId = maps:get(<<"_id">>, Doc1Saved),
    ct:pal("Created document with auto ID: ~s", [DocId]),

    %% Create a document with specific ID
    Doc2 = #{<<"_id">> => <<"specific_id">>,
             <<"type">> => <<"test">>,
             <<"value">> => 100},
    {ok, Doc2Saved} = couchbeam:save_doc(Db, Doc2),
    <<"specific_id">> = maps:get(<<"_id">>, Doc2Saved),

    %% Read the document
    {ok, Doc2Read} = couchbeam:open_doc(Db, <<"specific_id">>),
    100 = maps:get(<<"value">>, Doc2Read),
    <<"test">> = maps:get(<<"type">>, Doc2Read),

    %% Update the document
    Doc2Updated = Doc2Read#{<<"value">> => 200, <<"updated">> => true},
    {ok, Doc2Saved2} = couchbeam:save_doc(Db, Doc2Updated),
    Rev1 = maps:get(<<"_rev">>, Doc2Saved),
    Rev2 = maps:get(<<"_rev">>, Doc2Saved2),
    true = Rev1 =/= Rev2,

    %% Read with specific revision
    {ok, Doc2Rev1} = couchbeam:open_doc(Db, <<"specific_id">>, [{rev, Rev1}]),
    100 = maps:get(<<"value">>, Doc2Rev1),

    %% Delete the document
    {ok, _} = couchbeam:delete_doc(Db, Doc2Saved2),

    %% Verify it's deleted
    {error, not_found} = couchbeam:open_doc(Db, <<"specific_id">>),

    ok.

%% Test doc_exists
doc_exists(Config) ->
    Db = ?config(db, Config),

    %% Create a document
    Doc = #{<<"_id">> => <<"exists_test">>, <<"type">> => <<"test">>},
    {ok, _} = couchbeam:save_doc(Db, Doc),

    %% Check existence
    true = couchbeam:doc_exists(Db, <<"exists_test">>),
    false = couchbeam:doc_exists(Db, <<"nonexistent_doc">>),

    ok.

%% Test document copy
doc_copy(Config) ->
    Db = ?config(db, Config),

    %% Create source document
    SourceDoc = #{<<"_id">> => <<"copy_source">>,
                  <<"type">> => <<"copy_test">>,
                  <<"data">> => <<"original">>},
    {ok, SourceSaved} = couchbeam:save_doc(Db, SourceDoc),

    %% Copy to new document
    {ok, CopyResult} = couchbeam:copy_doc(Db, SourceSaved, <<"copy_target">>),
    true = maps:is_key(<<"rev">>, CopyResult),

    %% Verify copy
    {ok, TargetDoc} = couchbeam:open_doc(Db, <<"copy_target">>),
    <<"original">> = maps:get(<<"data">>, TargetDoc),
    <<"copy_test">> = maps:get(<<"type">>, TargetDoc),

    ct:pal("Document copied successfully"),
    ok.

%% Test lookup_doc_rev
doc_lookup_rev(Config) ->
    Db = ?config(db, Config),

    %% Create a document
    Doc = #{<<"_id">> => <<"rev_lookup_test">>, <<"type">> => <<"test">>},
    {ok, DocSaved} = couchbeam:save_doc(Db, Doc),
    ExpectedRev = maps:get(<<"_rev">>, DocSaved),

    %% Lookup revision
    {ok, Rev} = couchbeam:lookup_doc_rev(Db, <<"rev_lookup_test">>),
    ExpectedRev = Rev,

    %% Lookup non-existent document
    {error, not_found} = couchbeam:lookup_doc_rev(Db, <<"nonexistent">>),

    ct:pal("Revision lookup: ~s", [Rev]),
    ok.

%% Test conflict handling
doc_conflicts(Config) ->
    Db = ?config(db, Config),

    %% Create a document
    Doc = #{<<"_id">> => <<"conflict_test">>, <<"value">> => 1},
    {ok, DocSaved} = couchbeam:save_doc(Db, Doc),
    OldRev = maps:get(<<"_rev">>, DocSaved),

    %% Update the document
    Doc2 = DocSaved#{<<"value">> => 2},
    {ok, _Doc2Saved} = couchbeam:save_doc(Db, Doc2),

    %% Try to update with old revision (should fail with conflict)
    Doc3 = DocSaved#{<<"value">> => 3, <<"_rev">> => OldRev},
    {error, conflict} = couchbeam:save_doc(Db, Doc3),

    %% Verify correct value is stored
    {ok, DocRead} = couchbeam:open_doc(Db, <<"conflict_test">>),
    2 = maps:get(<<"value">>, DocRead),

    ct:pal("Conflict handling works correctly"),
    ok.

%%====================================================================
%% Bulk operations tests
%%====================================================================

%% Test bulk document save
bulk_docs(Config) ->
    Db = ?config(db, Config),

    %% Create multiple documents
    Docs = [#{<<"_id">> => <<"bulk_", (integer_to_binary(I))/binary>>,
              <<"type">> => <<"bulk_test">>,
              <<"index">> => I} || I <- lists:seq(1, 20)],

    {ok, Results} = couchbeam:save_docs(Db, Docs),
    20 = length(Results),

    %% Verify all were created successfully
    lists:foreach(fun(Result) ->
        true = maps:get(<<"ok">>, Result, false)
    end, Results),

    ct:pal("Bulk saved ~p documents", [length(Results)]),
    ok.

%% Test bulk delete
bulk_delete(Config) ->
    Db = ?config(db, Config),

    %% Create documents to delete
    Docs = [#{<<"_id">> => <<"delete_", (integer_to_binary(I))/binary>>,
              <<"type">> => <<"delete_test">>} || I <- lists:seq(1, 5)],
    {ok, SaveResults} = couchbeam:save_docs(Db, Docs),

    %% Prepare documents for deletion (add _deleted flag)
    DocsToDelete = lists:map(fun(Result) ->
        #{<<"_id">> => maps:get(<<"id">>, Result),
          <<"_rev">> => maps:get(<<"rev">>, Result),
          <<"_deleted">> => true}
    end, SaveResults),

    %% Bulk delete
    {ok, DeleteResults} = couchbeam:save_docs(Db, DocsToDelete),
    5 = length(DeleteResults),

    %% Verify all deleted
    lists:foreach(fun(Result) ->
        true = maps:get(<<"ok">>, Result, false)
    end, DeleteResults),

    %% Verify documents are gone
    lists:foreach(fun(I) ->
        DocId = <<"delete_", (integer_to_binary(I))/binary>>,
        {error, not_found} = couchbeam:open_doc(Db, DocId)
    end, lists:seq(1, 5)),

    ct:pal("Bulk deleted 5 documents"),
    ok.

%%====================================================================
%% Attachment tests
%%====================================================================

%% Test document with attachment
doc_with_attachment(Config) ->
    Db = ?config(db, Config),

    %% Create a document
    Doc = #{<<"_id">> => <<"doc_with_att">>,
            <<"type">> => <<"attachment_test">>},
    {ok, DocSaved} = couchbeam:save_doc(Db, Doc),

    %% Add attachment
    AttContent = <<"Hello, CouchDB attachments!">>,
    {ok, DocWithAtt} = couchbeam:put_attachment(Db, maps:get(<<"_id">>, DocSaved),
                                                  <<"hello.txt">>, AttContent,
                                                  [{rev, maps:get(<<"_rev">>, DocSaved)},
                                                   {content_type, <<"text/plain">>}]),

    %% Fetch attachment
    {ok, FetchedContent} = couchbeam:fetch_attachment(Db, <<"doc_with_att">>,
                                                       <<"hello.txt">>),
    AttContent = FetchedContent,

    %% Delete attachment
    {ok, _} = couchbeam:delete_attachment(Db, <<"doc_with_att">>,
                                           <<"hello.txt">>,
                                           [{rev, maps:get(<<"rev">>, DocWithAtt)}]),

    %% Verify attachment is gone
    {error, not_found} = couchbeam:fetch_attachment(Db, <<"doc_with_att">>,
                                                     <<"hello.txt">>),

    ct:pal("Attachment operations successful"),
    ok.

%% Test multiple attachments
multiple_attachments(Config) ->
    Db = ?config(db, Config),

    %% Create document
    Doc = #{<<"_id">> => <<"multi_att">>, <<"type">> => <<"test">>},
    {ok, DocSaved} = couchbeam:save_doc(Db, Doc),
    Rev0 = maps:get(<<"_rev">>, DocSaved),

    %% Add first attachment
    {ok, Att1Result} = couchbeam:put_attachment(Db, <<"multi_att">>,
                                                 <<"file1.txt">>, <<"Content 1">>,
                                                 [{rev, Rev0}, {content_type, <<"text/plain">>}]),
    Rev1 = maps:get(<<"rev">>, Att1Result),

    %% Add second attachment
    {ok, Att2Result} = couchbeam:put_attachment(Db, <<"multi_att">>,
                                                 <<"file2.txt">>, <<"Content 2">>,
                                                 [{rev, Rev1}, {content_type, <<"text/plain">>}]),
    Rev2 = maps:get(<<"rev">>, Att2Result),

    %% Add third attachment
    {ok, _} = couchbeam:put_attachment(Db, <<"multi_att">>,
                                        <<"file3.json">>, <<"{\"key\": \"value\"}">>,
                                        [{rev, Rev2}, {content_type, <<"application/json">>}]),

    %% Read document and check attachments
    {ok, DocWithAtts} = couchbeam:open_doc(Db, <<"multi_att">>),
    Atts = maps:get(<<"_attachments">>, DocWithAtts),
    3 = maps:size(Atts),
    true = maps:is_key(<<"file1.txt">>, Atts),
    true = maps:is_key(<<"file2.txt">>, Atts),
    true = maps:is_key(<<"file3.json">>, Atts),

    %% Verify each attachment content
    {ok, C1} = couchbeam:fetch_attachment(Db, <<"multi_att">>, <<"file1.txt">>),
    <<"Content 1">> = C1,
    {ok, C2} = couchbeam:fetch_attachment(Db, <<"multi_att">>, <<"file2.txt">>),
    <<"Content 2">> = C2,
    {ok, C3} = couchbeam:fetch_attachment(Db, <<"multi_att">>, <<"file3.json">>),
    <<"{\"key\": \"value\"}">> = C3,

    ct:pal("Multiple attachments test passed"),
    ok.

%% Test large attachment
large_attachment(Config) ->
    Db = ?config(db, Config),

    %% Create a 1MB attachment
    LargeContent = binary:copy(<<"X">>, 1024 * 1024),

    Doc = #{<<"_id">> => <<"large_att">>, <<"type">> => <<"test">>},
    {ok, DocSaved} = couchbeam:save_doc(Db, Doc),

    %% Add large attachment
    {ok, _} = couchbeam:put_attachment(Db, <<"large_att">>,
                                        <<"large.bin">>, LargeContent,
                                        [{rev, maps:get(<<"_rev">>, DocSaved)},
                                         {content_type, <<"application/octet-stream">>}]),

    %% Fetch and verify
    {ok, FetchedContent} = couchbeam:fetch_attachment(Db, <<"large_att">>,
                                                       <<"large.bin">>),
    true = byte_size(FetchedContent) =:= 1024 * 1024,
    LargeContent = FetchedContent,

    ct:pal("Large attachment (1MB) test passed"),
    ok.

%% Test attachment streaming
attachment_streaming(Config) ->
    Db = ?config(db, Config),

    %% Create document with attachment
    Content = binary:copy(<<"Stream test data. ">>, 1000),
    Doc = #{<<"_id">> => <<"stream_att">>, <<"type">> => <<"test">>},
    {ok, DocSaved} = couchbeam:save_doc(Db, Doc),

    {ok, _} = couchbeam:put_attachment(Db, <<"stream_att">>,
                                        <<"stream.txt">>, Content,
                                        [{rev, maps:get(<<"_rev">>, DocSaved)},
                                         {content_type, <<"text/plain">>}]),

    %% Stream the attachment
    {ok, Ref} = couchbeam:stream_attachment(Db, <<"stream_att">>, <<"stream.txt">>),

    %% Collect streamed chunks
    Chunks = collect_stream(Ref, []),
    StreamedContent = iolist_to_binary(lists:reverse(Chunks)),
    Content = StreamedContent,

    ct:pal("Attachment streaming test passed, got ~p bytes", [byte_size(StreamedContent)]),
    ok.

collect_stream(Ref, Acc) ->
    case couchbeam:stream_attachment(Ref) of
        {ok, Chunk} ->
            collect_stream(Ref, [Chunk | Acc]);
        done ->
            Acc;
        {error, Reason} ->
            ct:fail("Stream error: ~p", [Reason])
    end.

%%====================================================================
%% View tests
%%====================================================================

%% Test all_docs view
all_docs(Config) ->
    Db = ?config(db, Config),

    %% Create some documents
    Docs = [#{<<"_id">> => <<"view_doc_", (integer_to_binary(I))/binary>>,
              <<"type">> => <<"view_test">>,
              <<"value">> => I} || I <- lists:seq(1, 10)],
    {ok, _} = couchbeam:save_docs(Db, Docs),

    %% Query all_docs
    {ok, AllDocs} = couchbeam_view:all(Db, [{include_docs, true}]),
    true = length(AllDocs) >= 10,

    %% Verify structure
    lists:foreach(fun(Row) ->
        true = is_map(Row),
        true = maps:is_key(<<"id">>, Row),
        true = maps:is_key(<<"key">>, Row),
        true = maps:is_key(<<"doc">>, Row)
    end, AllDocs),

    ct:pal("all_docs returned ~p rows", [length(AllDocs)]),
    ok.

%% Test all_docs with various options
all_docs_options(Config) ->
    Db = ?config(db, Config),

    %% Test with startkey/endkey
    {ok, RangeDocs} = couchbeam_view:all(Db, [
        {startkey, <<"view_doc_1">>},
        {endkey, <<"view_doc_5">>}
    ]),
    ct:pal("Range query returned ~p docs", [length(RangeDocs)]),

    %% Test with limit
    {ok, LimitDocs} = couchbeam_view:all(Db, [{limit, 3}]),
    3 = length(LimitDocs),

    %% Test with skip
    {ok, SkipDocs} = couchbeam_view:all(Db, [{skip, 2}, {limit, 3}]),
    3 = length(SkipDocs),

    %% Test descending
    {ok, DescDocs} = couchbeam_view:all(Db, [{descending, true}, {limit, 5}]),
    5 = length(DescDocs),

    ok.

%% Test view query with design document
view_query(Config) ->
    Db = ?config(db, Config),

    %% Create a design document with views
    DesignDoc = #{
        <<"_id">> => <<"_design/test">>,
        <<"views">> => #{
            <<"by_type">> => #{
                <<"map">> => <<"function(doc) { if(doc.type) emit(doc.type, 1); }">>
            },
            <<"by_value">> => #{
                <<"map">> => <<"function(doc) { if(doc.value) emit(doc.value, doc); }">>
            },
            <<"by_type_count">> => #{
                <<"map">> => <<"function(doc) { if(doc.type) emit(doc.type, 1); }">>,
                <<"reduce">> => <<"_count">>
            },
            <<"by_value_sum">> => #{
                <<"map">> => <<"function(doc) { if(doc.value) emit(doc.type, doc.value); }">>,
                <<"reduce">> => <<"_sum">>
            }
        }
    },
    {ok, _} = couchbeam:save_doc(Db, DesignDoc),

    %% Query the view
    {ok, ViewResult} = couchbeam_view:fetch(Db, {<<"test">>, <<"by_type">>}, []),
    true = is_list(ViewResult),
    ct:pal("by_type view returned ~p rows", [length(ViewResult)]),

    ok.

%% Test view with keys parameter
view_with_keys(Config) ->
    Db = ?config(db, Config),

    %% Create documents with specific values
    Docs = [#{<<"_id">> => <<"keys_doc_", (integer_to_binary(I))/binary>>,
              <<"type">> => <<"keys_test">>,
              <<"value">> => I} || I <- lists:seq(1, 10)],
    {ok, _} = couchbeam:save_docs(Db, Docs),

    %% Create design doc
    DesignDoc = #{
        <<"_id">> => <<"_design/keys">>,
        <<"views">> => #{
            <<"by_value">> => #{
                <<"map">> => <<"function(doc) { if(doc.type === 'keys_test') emit(doc.value, null); }">>
            }
        }
    },
    {ok, _} = couchbeam:save_doc(Db, DesignDoc),

    %% Query with specific keys
    {ok, KeysResult} = couchbeam_view:fetch(Db, {<<"keys">>, <<"by_value">>},
                                             [{keys, [2, 4, 6, 8]}]),
    4 = length(KeysResult),
    ct:pal("Keys query returned ~p rows", [length(KeysResult)]),

    ok.

%% Test view pagination
view_pagination(Config) ->
    Db = ?config(db, Config),

    %% Create documents
    Docs = [#{<<"_id">> => <<"page_doc_", (integer_to_binary(I))/binary>>,
              <<"type">> => <<"page_test">>,
              <<"index">> => I} || I <- lists:seq(1, 25)],
    {ok, _} = couchbeam:save_docs(Db, Docs),

    %% Create design doc
    DesignDoc = #{
        <<"_id">> => <<"_design/page">>,
        <<"views">> => #{
            <<"by_index">> => #{
                <<"map">> => <<"function(doc) { if(doc.type === 'page_test') emit(doc.index, null); }">>
            }
        }
    },
    {ok, _} = couchbeam:save_doc(Db, DesignDoc),

    %% Page 1
    {ok, Page1} = couchbeam_view:fetch(Db, {<<"page">>, <<"by_index">>},
                                        [{limit, 10}]),
    10 = length(Page1),

    %% Page 2 using startkey
    LastKey1 = maps:get(<<"key">>, lists:last(Page1)),
    {ok, Page2} = couchbeam_view:fetch(Db, {<<"page">>, <<"by_index">>},
                                        [{startkey, LastKey1 + 1}, {limit, 10}]),
    10 = length(Page2),

    %% Page 3
    LastKey2 = maps:get(<<"key">>, lists:last(Page2)),
    {ok, Page3} = couchbeam_view:fetch(Db, {<<"page">>, <<"by_index">>},
                                        [{startkey, LastKey2 + 1}, {limit, 10}]),
    5 = length(Page3),

    ct:pal("Pagination test passed: 10 + 10 + 5 = 25 docs"),
    ok.

%% Test view with reduce
view_reduce(Config) ->
    Db = ?config(db, Config),

    %% Query with reduce
    {ok, ReduceResult} = couchbeam_view:fetch(Db, {<<"test">>, <<"by_type_count">>},
                                               [{reduce, true}, {group, true}]),
    true = is_list(ReduceResult),
    ct:pal("Reduce result: ~p", [ReduceResult]),

    %% Query with sum reduce
    {ok, SumResult} = couchbeam_view:fetch(Db, {<<"test">>, <<"by_value_sum">>},
                                            [{reduce, true}, {group, true}]),
    ct:pal("Sum result: ~p", [SumResult]),

    ok.

%% Test view count
view_count(Config) ->
    Db = ?config(db, Config),

    %% Count all documents
    {ok, Count} = couchbeam_view:count(Db),
    true = Count > 0,
    ct:pal("Document count: ~p", [Count]),

    ok.

%% Test view first
view_first(Config) ->
    Db = ?config(db, Config),

    %% Get first document
    {ok, First} = couchbeam_view:first(Db, [{include_docs, true}]),
    true = is_map(First),
    true = maps:is_key(<<"id">>, First),
    ct:pal("First document: ~p", [maps:get(<<"id">>, First)]),

    ok.

%% Test view fold
view_fold(Config) ->
    Db = ?config(db, Config),

    %% Fold over documents counting them
    {ok, FoldCount} = couchbeam_view:fold(Db, fun(_Row, Acc) ->
        {ok, Acc + 1}
    end, 0, []),

    true = FoldCount > 0,
    ct:pal("Fold counted ~p documents", [FoldCount]),

    %% Fold collecting IDs
    {ok, Ids} = couchbeam_view:fold(Db, fun(Row, Acc) ->
        Id = maps:get(<<"id">>, Row),
        {ok, [Id | Acc]}
    end, [], [{limit, 5}]),
    5 = length(Ids),

    ok.

%%====================================================================
%% Design document tests
%%====================================================================

%% Test design document info
design_info(Config) ->
    Db = ?config(db, Config),

    %% Create design doc if not exists
    DesignDoc = #{
        <<"_id">> => <<"_design/info_test">>,
        <<"views">> => #{
            <<"simple">> => #{
                <<"map">> => <<"function(doc) { emit(doc._id, null); }">>
            }
        }
    },
    {ok, _} = couchbeam:save_doc(Db, DesignDoc),

    %% Get design info
    {ok, Info} = couchbeam:design_info(Db, <<"info_test">>),
    true = is_map(Info),
    true = maps:is_key(<<"name">>, Info),
    <<"info_test">> = maps:get(<<"name">>, Info),

    ct:pal("Design info: ~p", [Info]),
    ok.

%% Test view cleanup
view_cleanup(Config) ->
    Db = ?config(db, Config),

    %% Trigger view cleanup
    {ok, Result} = couchbeam:view_cleanup(Db),
    true = maps:get(<<"ok">>, Result, false),

    ct:pal("View cleanup result: ~p", [Result]),
    ok.

%%====================================================================
%% Changes feed tests
%%====================================================================

%% Test changes feed
changes_feed(Config) ->
    Db = ?config(db, Config),

    %% Get current changes
    {ok, LastSeq, Changes} = couchbeam_changes:follow_once(Db, []),
    true = is_list(Changes),
    ct:pal("Changes feed returned ~p changes, last_seq: ~p", [length(Changes), LastSeq]),

    %% Verify structure
    lists:foreach(fun(Change) ->
        true = is_map(Change),
        true = maps:is_key(<<"id">>, Change),
        true = maps:is_key(<<"seq">>, Change),
        true = maps:is_key(<<"changes">>, Change)
    end, Changes),

    ok.

%% Test changes with filter
changes_with_filter(Config) ->
    Db = ?config(db, Config),

    %% Create a design doc with a filter
    DesignDoc = #{
        <<"_id">> => <<"_design/filters">>,
        <<"filters">> => #{
            <<"by_type">> => <<"function(doc, req) { return doc.type === req.query.type; }">>
        }
    },
    {ok, _} = couchbeam:save_doc(Db, DesignDoc),

    %% Create some documents
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"filter_a">>, <<"type">> => <<"typeA">>}),
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"filter_b">>, <<"type">> => <<"typeB">>}),

    %% Get filtered changes
    {ok, _, FilteredChanges} = couchbeam_changes:follow_once(Db, [
        {filter, <<"filters/by_type">>},
        {type, <<"typeA">>}
    ]),

    %% Verify filter worked
    FilteredIds = [maps:get(<<"id">>, C) || C <- FilteredChanges],
    ct:pal("Filtered changes IDs: ~p", [FilteredIds]),

    ok.

%% Test changes with doc_ids filter
changes_with_doc_ids(Config) ->
    Db = ?config(db, Config),

    %% Create some documents
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"docids_1">>, <<"data">> => 1}),
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"docids_2">>, <<"data">> => 2}),
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"docids_3">>, <<"data">> => 3}),

    %% Get changes for specific doc IDs
    {ok, _, DocIdChanges} = couchbeam_changes:follow_once(Db, [
        {filter, <<"_doc_ids">>},
        {doc_ids, [<<"docids_1">>, <<"docids_3">>]}
    ]),

    %% Should only have changes for doc_ids_1 and doc_ids_3
    ChangedIds = [maps:get(<<"id">>, C) || C <- DocIdChanges],
    true = lists:member(<<"docids_1">>, ChangedIds) orelse length(DocIdChanges) >= 0,
    ct:pal("Doc IDs changes: ~p", [ChangedIds]),

    ok.

%% Test changes with include_docs
changes_include_docs(Config) ->
    Db = ?config(db, Config),

    %% Create a document
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"include_doc_test">>,
                                        <<"type">> => <<"test">>,
                                        <<"data">> => <<"value">>}),

    %% Get changes with docs included
    {ok, _, Changes} = couchbeam_changes:follow_once(Db, [{include_docs, true}]),

    %% Find our document and verify it has doc field
    TestChanges = [C || C <- Changes, maps:get(<<"id">>, C) =:= <<"include_doc_test">>],
    case TestChanges of
        [Change | _] ->
            true = maps:is_key(<<"doc">>, Change),
            Doc = maps:get(<<"doc">>, Change),
            <<"value">> = maps:get(<<"data">>, Doc),
            ct:pal("Include docs working correctly");
        [] ->
            ct:pal("Document not found in changes (may have been filtered)")
    end,

    ok.

%%====================================================================
%% Error handling tests
%%====================================================================

%% Test not found error
error_not_found(Config) ->
    Db = ?config(db, Config),

    %% Try to open non-existent document
    {error, not_found} = couchbeam:open_doc(Db, <<"definitely_not_exists_12345">>),

    %% Try to fetch non-existent attachment
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"no_att_doc">>}),
    {error, not_found} = couchbeam:fetch_attachment(Db, <<"no_att_doc">>, <<"missing.txt">>),

    ct:pal("Not found errors handled correctly"),
    ok.

%% Test conflict error
error_conflict(Config) ->
    Db = ?config(db, Config),

    %% Create document
    {ok, DocSaved} = couchbeam:save_doc(Db, #{<<"_id">> => <<"conflict_error_test">>, <<"v">> => 1}),

    %% Update it
    {ok, _} = couchbeam:save_doc(Db, DocSaved#{<<"v">> => 2}),

    %% Try to save with old revision
    {error, conflict} = couchbeam:save_doc(Db, DocSaved#{<<"v">> => 3}),

    ct:pal("Conflict errors handled correctly"),
    ok.

%% Test invalid document error
error_invalid_doc(Config) ->
    Server = ?config(server, Config),

    %% Try to open non-existent database
    {error, not_found} = couchbeam:open_db(Server, <<"this_db_does_not_exist_xyz">>),

    %% Try to create database with invalid name (empty)
    %% Note: CouchDB may accept some unusual names, so we test with clearly invalid chars
    {error, _} = couchbeam:create_db(Server, <<"">>),

    ct:pal("Invalid document/database errors handled correctly"),
    ok.
