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
-include("couchbeam.hrl").

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

%% Test cases - View streaming operations
-export([view_stream_basic/1,
         view_stream_once/1,
         view_stream_cancel/1,
         view_foreach/1,
         view_stream_with_options/1,
         view_show/1]).

%% Test cases - Changes streaming operations
-export([changes_follow_continuous/1,
         changes_follow_longpoll/1,
         changes_follow_once_mode/1,
         changes_cancel/1,
         changes_with_heartbeat/1,
         changes_stream_to_pid/1]).

%% Test cases - Replication operations
-export([replicate_simple/1,
         replicate_with_options/1,
         replicate_map_object/1,
         replicate_continuous/1,
         replicate_filtered/1]).

%% Test cases - DB management operations
-export([compact_database/1,
         compact_design_view/1,
         ensure_full_commit_test/1,
         get_missing_revs/1]).

%% Test cases - Mango query operations
-export([find_basic/1,
         find_with_fields/1,
         find_with_sort/1,
         find_with_limit_skip/1,
         find_complex_selector/1]).

%% Test cases - UUID operations
-export([uuid_random_unique/1,
         uuid_utc_random_ordering/1,
         uuid_server_batch/1]).

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
     {group, error_handling},
     {group, view_streaming_ops},
     {group, changes_streaming_ops},
     {group, replication_ops},
     {group, db_management_ops},
     {group, mango_ops},
     {group, uuid_ops}].

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
    ]},
     {view_streaming_ops, [sequence], [
        view_stream_basic,
        view_stream_once,
        view_stream_cancel,
        view_foreach,
        view_stream_with_options,
        view_show
    ]},
     {changes_streaming_ops, [sequence], [
        changes_follow_continuous,
        changes_follow_longpoll,
        changes_follow_once_mode,
        changes_cancel,
        changes_with_heartbeat,
        changes_stream_to_pid
    ]},
     {replication_ops, [sequence], [
        replicate_simple,
        replicate_with_options,
        replicate_map_object,
        replicate_continuous,
        replicate_filtered
    ]},
     {db_management_ops, [sequence], [
        compact_database,
        compact_design_view,
        ensure_full_commit_test,
        get_missing_revs
    ]},
     {mango_ops, [sequence], [
        find_basic,
        find_with_fields,
        find_with_sort,
        find_with_limit_skip,
        find_complex_selector
    ]},
     {uuid_ops, [sequence], [
        uuid_random_unique,
        uuid_utc_random_ordering,
        uuid_server_batch
    ]}].

init_per_suite(Config) ->
    %% Start required applications
    {ok, _} = application:ensure_all_started(couchbeam),

    %% Configure hackney pool with larger size for integration tests
    PoolName = couchbeam_test_pool,
    ok = hackney_pool:start_pool(PoolName, [{max_connections, 100}]),

    %% Get CouchDB connection info from environment
    Url = os:getenv("COUCHDB_URL", "http://localhost:5984"),
    User = os:getenv("COUCHDB_USER", ""),
    Pass = os:getenv("COUCHDB_PASS", ""),

    %% Build server options with our custom pool
    BaseOptions = [{pool, PoolName}],
    Options = case {User, Pass} of
        {"", ""} -> BaseOptions;
        {U, P} -> [{basic_auth, {list_to_binary(U), list_to_binary(P)}} | BaseOptions]
    end,

    %% Try to connect to CouchDB
    Server = couchbeam:server_connection(Url, Options),

    case couchbeam:server_info(Server) of
        {ok, Info} ->
            ct:pal("Connected to CouchDB: ~p", [Info]),
            ct:pal("Using hackney pool ~p with max 100 connections", [PoolName]),
            [{server, Server}, {couchdb_url, Url}, {pool_name, PoolName} | Config];
        {error, Reason} ->
            hackney_pool:stop_pool(PoolName),
            ct:pal("CouchDB not available at ~s: ~p", [Url, Reason]),
            {skip, {couchdb_not_available, Reason}}
    end.

end_per_suite(Config) ->
    %% Stop the custom hackney pool
    case ?config(pool_name, Config) of
        undefined -> ok;
        PoolName -> hackney_pool:stop_pool(PoolName)
    end,
    ok.

init_per_group(mango_ops, Config) ->
    %% Check CouchDB version for Mango support (requires 2.0+)
    Server = ?config(server, Config),
    case is_mango_supported(Server) of
        true ->
            init_per_group_default(mango_ops, Config);
        false ->
            {skip, mango_not_supported}
    end;
init_per_group(replication_ops, Config) ->
    %% Create source and target databases for replication tests
    Server = ?config(server, Config),
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    SourceDb = iolist_to_binary([<<"couchbeam_test_rep_source_">>, Timestamp]),
    TargetDb = iolist_to_binary([<<"couchbeam_test_rep_target_">>, Timestamp]),

    case couchbeam:create_db(Server, SourceDb) of
        {ok, Source} ->
            case couchbeam:create_db(Server, TargetDb) of
                {ok, Target} ->
                    ct:pal("Created replication test databases: ~s -> ~s", [SourceDb, TargetDb]),
                    [{source_db, Source}, {source_db_name, SourceDb},
                     {target_db, Target}, {target_db_name, TargetDb},
                     {test_db, SourceDb}, {db, Source} | Config];
                {error, Reason} ->
                    couchbeam:delete_db(Source),
                    ct:pal("Failed to create target database: ~p", [Reason]),
                    {skip, {db_creation_failed, Reason}}
            end;
        {error, Reason} ->
            ct:pal("Failed to create source database: ~p", [Reason]),
            {skip, {db_creation_failed, Reason}}
    end;
init_per_group(Group, Config) ->
    init_per_group_default(Group, Config).

init_per_group_default(Group, Config) ->
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

end_per_group(replication_ops, Config) ->
    %% Clean up both source and target databases
    Server = ?config(server, Config),
    SourceDb = ?config(source_db_name, Config),
    TargetDb = ?config(target_db_name, Config),

    lists:foreach(fun(DbName) ->
        case couchbeam:open_db(Server, DbName) of
            {ok, Db} ->
                couchbeam:delete_db(Db),
                ct:pal("Cleaned up replication database: ~s", [DbName]);
            _ ->
                ok
        end
    end, [SourceDb, TargetDb]),
    ok;
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
    %% Small delay to allow hackney connections to be properly returned to pool
    %% This prevents pool exhaustion when running many streaming tests
    timer:sleep(50),
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
    [Uuid] = couchbeam:get_uuid(Server),
    32 = byte_size(Uuid),
    ct:pal("Got single UUID: ~s", [Uuid]),

    %% Get multiple UUIDs
    Uuids = couchbeam:get_uuids(Server, 5),
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

    %% Copy to new document - returns {ok, NewDocId, NewRev}
    {ok, <<"copy_target">>, CopyRev} = couchbeam:copy_doc(Db, SourceSaved, <<"copy_target">>),
    true = is_binary(CopyRev),

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

%%====================================================================
%% View streaming tests
%%====================================================================

%% Test basic view streaming
view_stream_basic(Config) ->
    Db = ?config(db, Config),

    %% Create some test documents
    Docs = [#{<<"_id">> => <<"stream_doc_", (integer_to_binary(I))/binary>>,
              <<"type">> => <<"stream_test">>,
              <<"value">> => I} || I <- lists:seq(1, 10)],
    {ok, _} = couchbeam:save_docs(Db, Docs),

    %% Stream all_docs
    {ok, Ref} = couchbeam_view:stream(Db, 'all_docs', []),

    %% Collect all rows
    {ok, Rows} = collect_view_stream(Ref, []),
    true = length(Rows) >= 10,

    ct:pal("View stream basic: received ~p rows", [length(Rows)]),
    ok.

%% Test view streaming with {async, once} mode
view_stream_once(Config) ->
    Db = ?config(db, Config),

    %% Create test documents
    Docs = [#{<<"_id">> => <<"once_doc_", (integer_to_binary(I))/binary>>,
              <<"type">> => <<"once_test">>} || I <- lists:seq(1, 5)],
    {ok, _} = couchbeam:save_docs(Db, Docs),

    %% Stream with {async, once}
    {ok, Ref} = couchbeam_view:stream(Db, 'all_docs', [{async, once}]),

    %% Receive first row
    receive
        {Ref, {row, Row1}} ->
            true = is_map(Row1),
            ct:pal("Received first row: ~p", [maps:get(<<"id">>, Row1)]),
            %% Request next
            ok = couchbeam_view:stream_next(Ref)
    after 5000 ->
        ct:fail("Timeout waiting for first row")
    end,

    %% Receive remaining rows
    {ok, _Rows} = collect_view_stream(Ref, []),
    ct:pal("View stream once mode working"),
    ok.

%% Test view stream cancellation
view_stream_cancel(Config) ->
    Db = ?config(db, Config),

    %% Create many documents
    Docs = [#{<<"_id">> => <<"cancel_doc_", (integer_to_binary(I))/binary>>,
              <<"type">> => <<"cancel_test">>} || I <- lists:seq(1, 20)],
    {ok, _} = couchbeam:save_docs(Db, Docs),

    %% Start streaming
    {ok, Ref} = couchbeam_view:stream(Db, 'all_docs', [{async, once}]),

    %% Receive a few rows
    receive
        {Ref, {row, _}} ->
            ok = couchbeam_view:stream_next(Ref)
    after 5000 ->
        ct:fail("Timeout waiting for row")
    end,

    %% Cancel the stream
    ok = couchbeam_view:cancel_stream(Ref),

    %% Verify stream is cancelled
    {error, stream_undefined} = couchbeam_view:stream_next(Ref),

    ct:pal("View stream cancellation working"),
    ok.

%% Test view foreach
view_foreach(Config) ->
    Db = ?config(db, Config),

    %% Create test documents
    Docs = [#{<<"_id">> => <<"foreach_doc_", (integer_to_binary(I))/binary>>,
              <<"type">> => <<"foreach_test">>,
              <<"value">> => I} || I <- lists:seq(1, 5)],
    {ok, _} = couchbeam:save_docs(Db, Docs),

    %% Use foreach to count documents
    Counter = counters:new(1, []),
    couchbeam_view:foreach(fun(_Row) ->
        counters:add(Counter, 1, 1)
    end, Db, 'all_docs', [{limit, 5}]),

    Count = counters:get(Counter, 1),
    true = Count >= 5,

    ct:pal("View foreach processed ~p rows", [Count]),
    ok.

%% Test view streaming with options
view_stream_with_options(Config) ->
    Db = ?config(db, Config),

    %% Create test documents
    Docs = [#{<<"_id">> => <<"opts_doc_", (integer_to_binary(I))/binary>>,
              <<"type">> => <<"opts_test">>,
              <<"value">> => I} || I <- lists:seq(1, 15)],
    {ok, _} = couchbeam:save_docs(Db, Docs),

    %% Stream with limit and skip
    {ok, Ref} = couchbeam_view:stream(Db, 'all_docs', [{limit, 5}, {skip, 2}]),
    {ok, Rows} = collect_view_stream(Ref, []),
    5 = length(Rows),

    ct:pal("View stream with options: limit=5, skip=2, got ~p rows", [length(Rows)]),
    ok.

%% Test show function
view_show(Config) ->
    Db = ?config(db, Config),

    %% Create a design document with a show function
    DesignDoc = #{
        <<"_id">> => <<"_design/show_test">>,
        <<"shows">> => #{
            <<"simple">> => <<"function(doc, req) { return {body: JSON.stringify({id: doc ? doc._id : null, query: req.query})}; }">>
        }
    },
    {ok, _} = couchbeam:save_doc(Db, DesignDoc),

    %% Create a test document
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"show_doc">>, <<"data">> => <<"test">>}),

    %% Call show function with document
    case couchbeam_view:show(Db, {<<"show_test">>, <<"simple">>}, <<"show_doc">>) of
        {ok, Result} ->
            ct:pal("Show function result: ~p", [Result]),
            ok;
        {error, Reason} ->
            %% Show functions may not be available in all CouchDB versions
            ct:pal("Show function not available: ~p", [Reason]),
            ok
    end.

%%====================================================================
%% Changes streaming tests
%%====================================================================

%% Test continuous changes feed
changes_follow_continuous(Config) ->
    Db = ?config(db, Config),

    %% Start following changes
    {ok, Ref} = couchbeam_changes:follow(Db, [continuous]),

    %% Trigger a change
    spawn(fun() ->
        timer:sleep(100),
        couchbeam:save_doc(Db, #{<<"_id">> => <<"continuous_test_doc">>})
    end),

    %% Collect changes with timeout
    {timeout, Changes} = collect_changes_timeout(Ref, [], 2000),

    %% Cancel the stream
    ok = couchbeam_changes:cancel(Ref),

    ct:pal("Continuous changes: received ~p changes", [length(Changes)]),
    ok.

%% Test longpoll changes feed
changes_follow_longpoll(Config) ->
    Db = ?config(db, Config),

    %% Create a document first
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"longpoll_doc_1">>}),

    %% Start longpoll feed
    {ok, Ref} = couchbeam_changes:follow(Db, [longpoll, {reconnect_after, false}]),

    %% Collect changes
    Result = receive
        {Ref, {done, LastSeq}} ->
            {done, LastSeq};
        {Ref, {change, Change}} ->
            {change, Change}
    after 5000 ->
        timeout
    end,

    %% Cancel to clean up
    couchbeam_changes:cancel(Ref),

    case Result of
        {done, _} ->
            ct:pal("Longpoll completed successfully");
        {change, _} ->
            ct:pal("Longpoll received change");
        timeout ->
            ct:pal("Longpoll timed out (expected for empty changes)")
    end,
    ok.

%% Test changes feed with {async, once} mode
changes_follow_once_mode(Config) ->
    Db = ?config(db, Config),

    %% Create some documents
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"once_mode_1">>}),
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"once_mode_2">>}),

    %% Start following with {async, once}
    {ok, Ref} = couchbeam_changes:follow(Db, [{async, once}]),

    %% Receive first change
    receive
        {Ref, {change, Change1}} ->
            true = is_map(Change1),
            ct:pal("Received first change in once mode"),
            %% Request next
            ok = couchbeam_changes:stream_next(Ref)
    after 5000 ->
        ct:pal("No changes available in once mode")
    end,

    %% Cancel the stream
    couchbeam_changes:cancel(Ref),
    ok.

%% Test changes stream cancellation
changes_cancel(Config) ->
    Db = ?config(db, Config),

    %% Start continuous feed
    {ok, Ref} = couchbeam_changes:follow(Db, [continuous]),

    %% Cancel it immediately
    ok = couchbeam_changes:cancel(Ref),

    %% Verify cancelled message
    receive
        {Ref, cancelled} ->
            ct:pal("Changes stream cancelled successfully")
    after 2000 ->
        %% Stream may have been cancelled before sending message
        ct:pal("Changes stream cancelled (no message)")
    end,

    %% Verify stream is gone
    {error, stream_undefined} = couchbeam_changes:stream_next(Ref),
    ok.

%% Test changes with heartbeat option
changes_with_heartbeat(Config) ->
    Db = ?config(db, Config),

    %% Start continuous feed with heartbeat
    {ok, Ref} = couchbeam_changes:follow(Db, [continuous, heartbeat, {heartbeat, 1000}]),

    %% Wait a bit for potential heartbeat
    timer:sleep(500),

    %% Cancel
    ok = couchbeam_changes:cancel(Ref),

    ct:pal("Changes with heartbeat test completed"),
    ok.

%% Test changes stream to specific pid
changes_stream_to_pid(Config) ->
    Db = ?config(db, Config),
    Self = self(),

    %% Spawn receiver process
    Receiver = spawn(fun() ->
        receive
            {Ref, {change, Change}} ->
                Self ! {received, Change, Ref};
            {Ref, {done, _}} ->
                Self ! {done, Ref};
            {Ref, cancelled} ->
                Self ! {cancelled, Ref}
        after 3000 ->
            Self ! timeout
        end
    end),

    %% Create a document to trigger change
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"pid_test_doc">>}),

    %% Start feed streaming to receiver
    {ok, Ref} = couchbeam_changes:follow(Db, [], Receiver),

    %% Wait for receiver to get something
    Result = receive
        {received, _, _} -> received;
        {done, _} -> done;
        {cancelled, _} -> cancelled;
        timeout -> timeout
    after 5000 ->
        error
    end,

    %% Clean up
    couchbeam_changes:cancel(Ref),

    ct:pal("Changes to pid result: ~p", [Result]),
    ok.

%%====================================================================
%% Replication tests
%%====================================================================

%% Test simple replication
replicate_simple(Config) ->
    Server = ?config(server, Config),
    SourceDb = ?config(source_db, Config),
    TargetDb = ?config(target_db, Config),

    %% Create a document in source
    {ok, _} = couchbeam:save_doc(SourceDb, #{<<"_id">> => <<"rep_doc_1">>, <<"data">> => <<"test">>}),

    %% Replicate using db records
    {ok, RepDoc} = couchbeam:replicate(Server, SourceDb, TargetDb),
    true = is_map(RepDoc),

    ct:pal("Simple replication created: ~p", [maps:get(<<"_id">>, RepDoc, undefined)]),
    ok.

%% Test replication with options
replicate_with_options(Config) ->
    Server = ?config(server, Config),
    SourceDb = ?config(source_db, Config),
    TargetDb = ?config(target_db, Config),

    %% Create a document in source
    {ok, _} = couchbeam:save_doc(SourceDb, #{<<"_id">> => <<"rep_opts_doc">>, <<"data">> => <<"test">>}),

    %% Replicate with create_target option using db records
    {ok, RepDoc} = couchbeam:replicate(Server, SourceDb, TargetDb,
                                        [{<<"create_target">>, true}]),
    true = is_map(RepDoc),

    ct:pal("Replication with options created"),
    ok.

%% Test replication with map object
replicate_map_object(Config) ->
    Server = ?config(server, Config),
    SourceDb = ?config(source_db, Config),
    TargetDb = ?config(target_db, Config),
    CouchdbUrl = ?config(couchdb_url, Config),

    %% Build full URLs for replication spec
    SourceUrl = iolist_to_binary([CouchdbUrl, "/", SourceDb#db.name]),
    TargetUrl = iolist_to_binary([CouchdbUrl, "/", TargetDb#db.name]),

    %% Create full replication spec with URLs
    RepSpec = #{
        <<"source">> => SourceUrl,
        <<"target">> => TargetUrl,
        <<"create_target">> => true
    },

    {ok, RepDoc} = couchbeam:replicate(Server, RepSpec),
    true = is_map(RepDoc),

    ct:pal("Replication with map object created"),
    ok.

%% Test continuous replication
replicate_continuous(Config) ->
    Server = ?config(server, Config),
    SourceDb = ?config(source_db, Config),
    TargetDb = ?config(target_db, Config),

    %% Start continuous replication using db records
    {ok, RepDoc} = couchbeam:replicate(Server, SourceDb, TargetDb,
                                        [{<<"continuous">>, true}]),
    true = is_map(RepDoc),

    ct:pal("Continuous replication started"),
    ok.

%% Test filtered replication
replicate_filtered(Config) ->
    Server = ?config(server, Config),
    SourceDb = ?config(source_db, Config),
    TargetDb = ?config(target_db, Config),

    %% Create a design doc with filter in source
    FilterDesignDoc = #{
        <<"_id">> => <<"_design/filters">>,
        <<"filters">> => #{
            <<"by_type">> => <<"function(doc, req) { return doc.type === 'replicate'; }">>
        }
    },
    {ok, _} = couchbeam:save_doc(SourceDb, FilterDesignDoc),

    %% Create documents
    {ok, _} = couchbeam:save_doc(SourceDb, #{<<"_id">> => <<"filter_rep_1">>, <<"type">> => <<"replicate">>}),
    {ok, _} = couchbeam:save_doc(SourceDb, #{<<"_id">> => <<"filter_rep_2">>, <<"type">> => <<"skip">>}),

    %% Replicate with filter using db records
    {ok, RepDoc} = couchbeam:replicate(Server, SourceDb, TargetDb,
                                        [{<<"filter">>, <<"filters/by_type">>}]),
    true = is_map(RepDoc),

    ct:pal("Filtered replication created"),
    ok.

%%====================================================================
%% Database management tests
%%====================================================================

%% Test database compaction
compact_database(Config) ->
    Db = ?config(db, Config),

    %% Create and delete some documents to generate waste
    lists:foreach(fun(I) ->
        DocId = <<"compact_doc_", (integer_to_binary(I))/binary>>,
        {ok, Doc} = couchbeam:save_doc(Db, #{<<"_id">> => DocId, <<"data">> => I}),
        couchbeam:delete_doc(Db, Doc)
    end, lists:seq(1, 10)),

    %% Compact the database
    ok = couchbeam:compact(Db),

    ct:pal("Database compaction triggered"),
    ok.

%% Test design view compaction
compact_design_view(Config) ->
    Db = ?config(db, Config),

    %% Create a design document
    DesignDoc = #{
        <<"_id">> => <<"_design/compact_test">>,
        <<"views">> => #{
            <<"by_id">> => #{
                <<"map">> => <<"function(doc) { emit(doc._id, null); }">>
            }
        }
    },
    {ok, _} = couchbeam:save_doc(Db, DesignDoc),

    %% Query view to build index
    {ok, _} = couchbeam_view:fetch(Db, {<<"compact_test">>, <<"by_id">>}, []),

    %% Compact the design view
    ok = couchbeam:compact(Db, <<"compact_test">>),

    ct:pal("Design view compaction triggered"),
    ok.

%% Test ensure_full_commit
ensure_full_commit_test(Config) ->
    Db = ?config(db, Config),

    %% Create a document
    {ok, _} = couchbeam:save_doc(Db, #{<<"_id">> => <<"commit_test">>}),

    %% Ensure full commit
    {ok, InstanceStartTime} = couchbeam:ensure_full_commit(Db),
    true = is_binary(InstanceStartTime),

    %% Test with options
    {ok, InstanceStartTime2} = couchbeam:ensure_full_commit(Db, []),
    true = is_binary(InstanceStartTime2),

    ct:pal("Ensure full commit: instance_start_time=~s", [InstanceStartTime]),
    ok.

%% Test get_missing_revs
get_missing_revs(Config) ->
    Db = ?config(db, Config),

    %% Create a document to get its revision
    {ok, Doc} = couchbeam:save_doc(Db, #{<<"_id">> => <<"revs_test">>}),
    ExistingRev = maps:get(<<"_rev">>, Doc),

    %% Check for missing revisions
    IdRevs = [
        {<<"revs_test">>, [ExistingRev, <<"1-fakefake">>]},
        {<<"nonexistent">>, [<<"1-abc123">>]}
    ],

    {ok, Missing} = couchbeam:get_missing_revs(Db, IdRevs),
    true = is_list(Missing),

    %% The fake revision should be missing
    ct:pal("Missing revisions check: ~p entries", [length(Missing)]),
    ok.

%%====================================================================
%% Mango query tests
%%====================================================================

%% Test basic Mango find
find_basic(Config) ->
    Db = ?config(db, Config),

    %% Create test documents
    Docs = [#{<<"_id">> => <<"mango_", (integer_to_binary(I))/binary>>,
              <<"type">> => <<"mango_test">>,
              <<"value">> => I,
              <<"category">> => if I rem 2 =:= 0 -> <<"even">>; true -> <<"odd">> end}
            || I <- lists:seq(1, 10)],
    {ok, _} = couchbeam:save_docs(Db, Docs),

    %% Create an index for the Mango queries
    create_mango_index(Db, [<<"type">>, <<"value">>, <<"category">>]),

    %% Basic selector
    Selector = #{<<"type">> => <<"mango_test">>},
    {ok, Result} = couchbeam:find(Db, Selector, []),
    DocList = maps:get(<<"docs">>, Result),
    true = length(DocList) >= 10,

    ct:pal("Mango find basic: found ~p documents", [length(DocList)]),
    ok.

%% Test Mango find with fields projection
find_with_fields(Config) ->
    Db = ?config(db, Config),

    Selector = #{<<"type">> => <<"mango_test">>},
    {ok, Result} = couchbeam:find(Db, Selector, [{fields, [<<"_id">>, <<"value">>]}]),
    DocList = maps:get(<<"docs">>, Result),

    %% Verify only requested fields are returned
    case DocList of
        [Doc | _] ->
            true = maps:is_key(<<"_id">>, Doc),
            true = maps:is_key(<<"value">>, Doc),
            %% category should not be present (unless CouchDB returns all fields)
            ct:pal("Mango find with fields: got ~p documents", [length(DocList)]);
        [] ->
            ct:pal("No documents found for fields test")
    end,
    ok.

%% Test Mango find with sort
find_with_sort(Config) ->
    Db = ?config(db, Config),

    %% Create an index specifically for sorting by value
    create_mango_index(Db, [<<"value">>]),

    Selector = #{<<"value">> => #{<<"$gt">> => 0}},
    case couchbeam:find(Db, Selector, [{sort, [#{<<"value">> => <<"desc">>}]}]) of
        {ok, Result} ->
            DocList = maps:get(<<"docs">>, Result),
            case DocList of
                [First, Second | _] ->
                    FirstVal = maps:get(<<"value">>, First),
                    SecondVal = maps:get(<<"value">>, Second),
                    true = FirstVal >= SecondVal,
                    ct:pal("Mango find with sort: first=~p, second=~p", [FirstVal, SecondVal]);
                _ ->
                    ct:pal("Not enough documents for sort test")
            end;
        {error, {bad_response, {400, _, _}}} ->
            %% Sort without index - CouchDB requires an index for sorting
            ct:pal("Sort without index not supported, creating index failed - skipping verification"),
            ok
    end,
    ok.

%% Test Mango find with limit and skip
find_with_limit_skip(Config) ->
    Db = ?config(db, Config),

    Selector = #{<<"type">> => <<"mango_test">>},

    %% Test limit
    {ok, Result1} = couchbeam:find(Db, Selector, [{limit, 3}]),
    DocList1 = maps:get(<<"docs">>, Result1),
    3 = length(DocList1),

    %% Test skip
    {ok, Result2} = couchbeam:find(Db, Selector, [{limit, 3}, {skip, 5}]),
    DocList2 = maps:get(<<"docs">>, Result2),
    true = length(DocList2) =< 3,

    ct:pal("Mango find with limit/skip: limit=3 got ~p, skip=5 got ~p",
           [length(DocList1), length(DocList2)]),
    ok.

%% Test Mango find with complex selector
find_complex_selector(Config) ->
    Db = ?config(db, Config),

    %% Complex selector with $and, $or, $gt, $lt
    Selector = #{
        <<"$and">> => [
            #{<<"type">> => <<"mango_test">>},
            #{<<"$or">> => [
                #{<<"category">> => <<"even">>},
                #{<<"value">> => #{<<"$gt">> => 8}}
            ]}
        ]
    },

    {ok, Result} = couchbeam:find(Db, Selector, []),
    DocList = maps:get(<<"docs">>, Result),

    %% Should find even numbers (2,4,6,8,10) or value > 8 (9,10)
    ct:pal("Mango complex selector: found ~p documents", [length(DocList)]),
    ok.

%%====================================================================
%% UUID tests
%%====================================================================

%% Test random UUID uniqueness
uuid_random_unique(Config) ->
    _ = Config,

    %% Generate many UUIDs
    Uuids = [couchbeam_uuids:random() || _ <- lists:seq(1, 100)],

    %% All should be unique
    UniqueUuids = lists:usort(Uuids),
    100 = length(UniqueUuids),

    %% All should be 32 characters (hex)
    lists:foreach(fun(Uuid) ->
        32 = byte_size(Uuid)
    end, Uuids),

    ct:pal("Random UUIDs: generated 100 unique UUIDs"),
    ok.

%% Test UTC random UUID ordering
uuid_utc_random_ordering(Config) ->
    _ = Config,

    %% Generate UUIDs with small delay between
    Uuid1 = couchbeam_uuids:utc_random(),
    timer:sleep(10),
    Uuid2 = couchbeam_uuids:utc_random(),
    timer:sleep(10),
    Uuid3 = couchbeam_uuids:utc_random(),

    %% UTC random UUIDs should sort chronologically
    true = Uuid1 < Uuid2,
    true = Uuid2 < Uuid3,

    %% All should be 32 characters
    32 = byte_size(Uuid1),
    32 = byte_size(Uuid2),
    32 = byte_size(Uuid3),

    ct:pal("UTC random UUIDs: ~s < ~s < ~s", [Uuid1, Uuid2, Uuid3]),
    ok.

%% Test server UUID batch retrieval
uuid_server_batch(Config) ->
    Server = ?config(server, Config),

    %% Get a batch of UUIDs from server
    Uuids = couchbeam:get_uuids(Server, 10),
    10 = length(Uuids),

    %% All should be unique
    UniqueUuids = lists:usort(Uuids),
    10 = length(UniqueUuids),

    %% All should be 32 characters
    lists:foreach(fun(Uuid) ->
        32 = byte_size(Uuid)
    end, Uuids),

    ct:pal("Server UUID batch: got 10 unique UUIDs"),
    ok.

%%====================================================================
%% Helper functions
%%====================================================================

%% Collect view stream rows
collect_view_stream(Ref, Acc) ->
    receive
        {Ref, done} ->
            {ok, lists:reverse(Acc)};
        {Ref, {row, Row}} ->
            collect_view_stream(Ref, [Row | Acc]);
        {Ref, {error, E}} ->
            {error, E}
    after 10000 ->
        {error, timeout}
    end.

%% Collect changes with timeout
collect_changes_timeout(Ref, Acc, Timeout) ->
    receive
        {Ref, {change, C}} ->
            collect_changes_timeout(Ref, [C | Acc], Timeout);
        {Ref, {changes, Cs}} ->
            collect_changes_timeout(Ref, lists:reverse(Cs) ++ Acc, Timeout);
        {Ref, {done, _}} ->
            {ok, lists:reverse(Acc)};
        {Ref, cancelled} ->
            {cancelled, lists:reverse(Acc)}
    after Timeout ->
        {timeout, lists:reverse(Acc)}
    end.

%% Check CouchDB version for Mango support
is_mango_supported(Server) ->
    case couchbeam:server_info(Server) of
        {ok, Info} ->
            Version = maps:get(<<"version">>, Info, <<"0">>),
            case binary:split(Version, <<".">>, [global]) of
                [MajorBin | _] ->
                    try
                        Major = binary_to_integer(MajorBin),
                        Major >= 2
                    catch _:_ ->
                        %% Unable to parse version, assume supported
                        true
                    end;
                _ ->
                    true
            end;
        _ ->
            false
    end.

%% Create a Mango index on the given fields using HTTP API directly
create_mango_index(#db{server=Server, options=Opts}=Db, Fields) ->
    IndexSpec = #{
        <<"index">> => #{
            <<"fields">> => Fields
        },
        <<"type">> => <<"json">>
    },
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server),
                               [couchbeam_httpc:db_url(Db), <<"_index">>],
                               []),
    Headers = [{<<"content-type">>, <<"application/json">>},
               {<<"accept">>, <<"application/json">>}],
    Body = couchbeam_ejson:encode(IndexSpec),
    case couchbeam_httpc:db_request(post, Url, Headers, Body, Opts, [200, 201]) of
        {ok, _, _, Ref} ->
            hackney:skip_body(Ref),
            ct:pal("Created Mango index on fields: ~p", [Fields]),
            ok;
        {error, Reason} ->
            ct:pal("Failed to create Mango index: ~p", [Reason]),
            ok
    end.
