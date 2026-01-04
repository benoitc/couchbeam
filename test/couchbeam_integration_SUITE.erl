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

%% Test cases
-export([server_info/1,
         create_delete_db/1,
         doc_crud/1,
         doc_with_attachment/1,
         bulk_docs/1,
         all_docs/1,
         view_query/1,
         changes_feed/1]).

%%====================================================================
%% CT callbacks
%%====================================================================

all() ->
    [{group, integration}].

groups() ->
    [{integration, [sequence], [
        server_info,
        create_delete_db,
        doc_crud,
        doc_with_attachment,
        bulk_docs,
        all_docs,
        view_query,
        changes_feed
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

init_per_group(integration, Config) ->
    %% Create a unique test database name
    TestDb = iolist_to_binary([<<"couchbeam_test_">>,
                               integer_to_binary(erlang:system_time(millisecond))]),
    [{test_db, TestDb} | Config].

end_per_group(integration, Config) ->
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
%% Test cases
%%====================================================================

%% Test server info retrieval
server_info(Config) ->
    Server = ?config(server, Config),
    {ok, Info} = couchbeam:server_info(Server),
    true = is_map(Info),
    true = maps:is_key(<<"couchdb">>, Info),
    true = maps:is_key(<<"version">>, Info),
    ct:pal("Server info: ~p", [Info]),
    ok.

%% Test database creation and deletion
create_delete_db(Config) ->
    Server = ?config(server, Config),
    TestDb = ?config(test_db, Config),

    %% Create database
    {ok, Db} = couchbeam:create_db(Server, TestDb),
    ct:pal("Created database: ~s", [TestDb]),

    %% Verify it exists
    true = couchbeam:db_exists(Server, TestDb),

    %% Get database info
    {ok, DbInfo} = couchbeam:db_info(Db),
    true = is_map(DbInfo),
    TestDb = maps:get(<<"db_name">>, DbInfo),

    ok.

%% Test document CRUD operations
doc_crud(Config) ->
    Server = ?config(server, Config),
    TestDb = ?config(test_db, Config),
    {ok, Db} = couchbeam:open_db(Server, TestDb),

    %% Create a document
    Doc1 = #{<<"_id">> => <<"test_doc_1">>,
             <<"type">> => <<"test">>,
             <<"value">> => 42},
    {ok, Doc1Saved} = couchbeam:save_doc(Db, Doc1),
    true = is_map(Doc1Saved),
    true = maps:is_key(<<"_rev">>, Doc1Saved),
    ct:pal("Created document: ~p", [Doc1Saved]),

    %% Read the document
    {ok, Doc1Read} = couchbeam:open_doc(Db, <<"test_doc_1">>),
    42 = maps:get(<<"value">>, Doc1Read),

    %% Update the document
    Doc1Updated = Doc1Read#{<<"value">> => 100},
    {ok, Doc1Saved2} = couchbeam:save_doc(Db, Doc1Updated),
    Rev2 = maps:get(<<"_rev">>, Doc1Saved2),
    true = Rev2 =/= maps:get(<<"_rev">>, Doc1Saved),

    %% Delete the document
    {ok, _} = couchbeam:delete_doc(Db, Doc1Saved2),

    %% Verify it's deleted
    {error, not_found} = couchbeam:open_doc(Db, <<"test_doc_1">>),

    ok.

%% Test document with attachment
doc_with_attachment(Config) ->
    Server = ?config(server, Config),
    TestDb = ?config(test_db, Config),
    {ok, Db} = couchbeam:open_db(Server, TestDb),

    %% Create a document with inline attachment
    AttContent = <<"Hello, CouchDB!">>,
    Doc = #{<<"_id">> => <<"doc_with_att">>,
            <<"type">> => <<"attachment_test">>},
    {ok, DocSaved} = couchbeam:save_doc(Db, Doc),

    %% Add attachment
    {ok, DocWithAtt} = couchbeam:put_attachment(Db, maps:get(<<"_id">>, DocSaved),
                                                  <<"hello.txt">>, AttContent,
                                                  [{rev, maps:get(<<"_rev">>, DocSaved)},
                                                   {content_type, <<"text/plain">>}]),
    ct:pal("Added attachment: ~p", [DocWithAtt]),

    %% Fetch attachment
    {ok, FetchedContent} = couchbeam:fetch_attachment(Db, <<"doc_with_att">>,
                                                       <<"hello.txt">>),
    AttContent = FetchedContent,

    %% Delete attachment
    {ok, _} = couchbeam:delete_attachment(Db, <<"doc_with_att">>,
                                           <<"hello.txt">>,
                                           [{rev, maps:get(<<"rev">>, DocWithAtt)}]),

    ok.

%% Test bulk document operations
bulk_docs(Config) ->
    Server = ?config(server, Config),
    TestDb = ?config(test_db, Config),
    {ok, Db} = couchbeam:open_db(Server, TestDb),

    %% Create multiple documents
    Docs = [#{<<"_id">> => <<"bulk_", (integer_to_binary(I))/binary>>,
              <<"type">> => <<"bulk_test">>,
              <<"index">> => I} || I <- lists:seq(1, 10)],

    {ok, Results} = couchbeam:save_docs(Db, Docs),
    10 = length(Results),
    ct:pal("Bulk saved ~p documents", [length(Results)]),

    %% Verify all were created successfully
    lists:foreach(fun(Result) ->
        true = maps:get(<<"ok">>, Result, false)
    end, Results),

    ok.

%% Test all_docs view
all_docs(Config) ->
    Server = ?config(server, Config),
    TestDb = ?config(test_db, Config),
    {ok, Db} = couchbeam:open_db(Server, TestDb),

    %% Query all_docs using couchbeam_view:all/2
    {ok, AllDocs} = couchbeam_view:all(Db, [{include_docs, true}]),
    true = is_list(AllDocs),
    ct:pal("all_docs returned ~p rows", [length(AllDocs)]),

    %% Each row should have id, key, value, and doc
    lists:foreach(fun(Row) ->
        true = is_map(Row),
        true = maps:is_key(<<"id">>, Row),
        true = maps:is_key(<<"key">>, Row)
    end, AllDocs),

    ok.

%% Test view query with design document
view_query(Config) ->
    Server = ?config(server, Config),
    TestDb = ?config(test_db, Config),
    {ok, Db} = couchbeam:open_db(Server, TestDb),

    %% Create a design document with a simple view
    DesignDoc = #{
        <<"_id">> => <<"_design/test">>,
        <<"views">> => #{
            <<"by_type">> => #{
                <<"map">> => <<"function(doc) { if(doc.type) emit(doc.type, 1); }">>
            },
            <<"by_type_reduce">> => #{
                <<"map">> => <<"function(doc) { if(doc.type) emit(doc.type, 1); }">>,
                <<"reduce">> => <<"_count">>
            }
        }
    },
    {ok, _} = couchbeam:save_doc(Db, DesignDoc),
    ct:pal("Created design document"),

    %% Query the view using couchbeam_view:fetch/3
    {ok, ViewResult} = couchbeam_view:fetch(Db, {<<"test">>, <<"by_type">>}, []),
    true = is_list(ViewResult),
    ct:pal("View returned ~p rows", [length(ViewResult)]),

    %% Query with reduce
    {ok, ReduceResult} = couchbeam_view:fetch(Db, {<<"test">>, <<"by_type_reduce">>},
                                              [{reduce, true}, {group, true}]),
    ct:pal("Reduce result: ~p", [ReduceResult]),

    ok.

%% Test changes feed
changes_feed(Config) ->
    Server = ?config(server, Config),
    TestDb = ?config(test_db, Config),
    {ok, Db} = couchbeam:open_db(Server, TestDb),

    %% Get current changes using follow_once (should have entries from previous tests)
    {ok, LastSeq, Changes} = couchbeam_changes:follow_once(Db, []),
    true = is_list(Changes),
    ct:pal("Changes feed returned ~p changes, last_seq: ~p", [length(Changes), LastSeq]),

    %% Each change should have seq and id
    lists:foreach(fun(Change) ->
        true = is_map(Change),
        true = maps:is_key(<<"id">>, Change)
    end, Changes),

    %% Test changes with since parameter - should return empty since we're at the end
    {ok, _, ChangesEmpty} = couchbeam_changes:follow_once(Db, [{since, LastSeq}]),
    0 = length(ChangesEmpty),

    ok.
