%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam).
-author('Benoît Chesneau <benoitc@e-engura.org>').

-include("couchbeam.hrl").

-define(TIMEOUT, infinity).

%% API urls
-export([server_connection/0, server_connection/1,
         server_connection/2, server_connection/4,
         server_info/1,
         get_uuid/1, get_uuids/2,
         replicate/2, replicate/3, replicate/4,
         all_dbs/1, all_dbs/2, db_exists/2,
         create_db/2, create_db/3, create_db/4,
         open_db/2, open_db/3,
         open_or_create_db/2, open_or_create_db/3, open_or_create_db/4,
         delete_db/1, delete_db/2,
         db_info/1,
         design_info/2, view_cleanup/1,
         save_doc/2, save_doc/3, save_doc/4,
         doc_exists/2,
         open_doc/2, open_doc/3,
         stream_doc/1, end_doc_stream/1,
         delete_doc/2, delete_doc/3,
         save_docs/2, save_docs/3,
         delete_docs/2, delete_docs/3,
         copy_doc/2, copy_doc/3,
         lookup_doc_rev/2, lookup_doc_rev/3,
         fetch_attachment/3, fetch_attachment/4, stream_attachment/1,
         delete_attachment/3, delete_attachment/4,
         put_attachment/4, put_attachment/5, send_attachment/2,
         ensure_full_commit/1, ensure_full_commit/2,
         compact/1, compact/2,
         get_missing_revs/2]).

-opaque doc_stream() :: {atom(), any()}.
-export_type([doc_stream/0]).

-type mp_attachments() :: {Name :: binary(), Bin :: binary()}
    | {Name :: binary(), Bin :: binary(), Encoding :: binary()}
    | { Name :: binary(), Bin :: binary(), Type :: binary(), Encoding :: binary()}
    | { Name :: binary(), {file, Path ::  string()}}
    | { Name :: binary(), {file, Path ::  string()}, Encoding :: binary()}
    | { Name :: binary(), Fun :: fun(), Length :: integer()}
    | { Name :: binary(), Fun :: fun(), Length :: integer(), Encoding :: binary()}
    | { Name :: binary(), Fun :: fun(), Length :: integer(), Type :: binary(), Encoding :: binary()}
    | { Name :: binary(), {Fun :: fun(), Acc :: any()}, Length :: integer()}
    | { Name :: binary(), {Fun :: fun(), Acc :: any()}, Length :: integer(), Encoding :: binary()}
    | { Name :: binary(), {Fun :: fun(), Acc :: any()}, Length :: integer(), Type :: binary(), Encoding :: binary()}.


%% --------------------------------------------------------------------
%% API functins.
%% --------------------------------------------------------------------

%% @doc Create a server for connectiong to a CouchDB node
%% @equiv server_connection("127.0.0.1", 5984, "", [], false)
server_connection() ->
    server_connection(<<"http://127.0.0.1:5984">>, []).


server_connection(URL) when is_list(URL) orelse is_binary(URL) ->
    server_connection(URL, []).



%% @doc Create a server for connectiong to a CouchDB node
%% @equiv server_connection(Host, Port, "", [])

server_connection(URL, Options) when is_list(Options) ->
    #server{url=hackney_url:fix_path(URL), options=Options};
server_connection(Host, Port) when is_integer(Port) ->
    server_connection(Host, Port, "", []).


%% @doc Create a server for connectiong to a CouchDB node
%%
%%      Connections are made to:
%%      ```http://Host:PortPrefix'''
%%
%%      If ssl is set https is used.
%%
%%      For a description of SSL Options, look in the <a href="http://www.erlang.org/doc/apps/ssl/index.html">ssl</a> manpage.
%%
-spec server_connection(Host::string(), Port::non_neg_integer(), Prefix::string(), OptionsList::list()) ->
        Server::server().
%% OptionsList() = [option()]
%% option() =
%%          {is_ssl, boolean()}                |
%%          {ssl_options, [SSLOpt]}            |
%%          {pool_name, atom()}                |
%%          {proxy_host, string()}             |
%%          {proxy_port, integer()}            |
%%          {proxy_user, string()}             |
%%          {proxy_password, string()}         |
%%          {basic_auth, {username(), password()}} |
%%          {cookie, string()}                 |
%%          {oauth, oauthOptions()}            |
%%          {proxyauth, [proxyauthOpt]}
%%
%% username() = string()
%% password() = string()
%% SSLOpt = term()
%% oauthOptions() = [oauth()]
%% oauth() =
%%          {consumer_key, string()} |
%%          {token, string()} |
%%          {token_secret, string()} |
%%          {consumer_secret, string()} |
%%          {signature_method, string()}
%%
%% proxyOpt = {X-Auth-CouchDB-UserName, username :: string()} |
%%            {X-Auth-CouchDB-Roles, roles :: string} | list_of_user_roles_separated_by_a_comma
%%            {X-Auth-CouchDB-Token: token :: string()} | authentication token. Optional, but strongly recommended to force token be required to prevent requests from untrusted sources.




server_connection(Host, Port, Prefix, Options)
        when is_integer(Port), Port =:= 443 ->
    BaseUrl = iolist_to_binary(["https://", Host, ":",
                                integer_to_list(Port)]),
    Url = hackney_url:make_url(BaseUrl, [Prefix], []),
    server_connection(Url, Options);
server_connection(Host, Port, Prefix, Options) ->
    Scheme = case proplists:get_value(is_ssl, Options) of
        true -> "https";
        _ -> "http"
    end,

    BaseUrl = iolist_to_binary([Scheme, "://", Host, ":",
                                integer_to_list(Port)]),
    Url = hackney_url:make_url(BaseUrl, [Prefix], []),
    server_connection(Url, Options).

%% @doc Get Information from the server
%% @spec server_info(server()) -> {ok, iolist()}
server_info(#server{url=Url, options=Opts}) ->
    case hackney:get(Url, [], <<>>, Opts) of
        {ok, 200, _, Ref} ->
            Version = couchbeam_httpc:json_body(Ref),
            {ok, Version};
        {ok, Status, Headers, Ref} ->
            {ok, Body} = hackney:body(Ref),
            {error, {bad_response, {Status, Headers, Body}}};

        Error ->
            Error
    end.

%% @doc Get one uuid from the server
%% @spec get_uuid(server()) -> lists()
get_uuid(Server) ->
    couchbeam_uuids:get_uuids(Server, 1).

%% @doc Get a list of uuids from the server
%% @spec get_uuids(server(), integer()) -> lists()
get_uuids(Server, Count) ->
    couchbeam_uuids:get_uuids(Server, Count).


%% @doc Handle replication. Pass an object containting all informations
%% It allows to pass for example an authentication info
%% ```
%% RepObj = {[
%% {<<"source">>, <<"sourcedb">>},
%% {<<"target">>, <<"targetdb">>},
%% {<<"create_target">>, true}
%% ]}
%% replicate(Server, RepObj).
%% '''
%%
%% @spec replicate(Server::server(), RepObj::{list()})
%%          -> {ok, Result}|{error, Error}
replicate(#server{url=ServerUrl, options=Opts}, RepObj) ->
    Url = hackney_url:make_url(ServerUrl, [<<"_replicate">>], []),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    JsonObj = couchbeam_ejson:encode(RepObj),

     case couchbeam_httpc:request(post, Url, Headers, JsonObj, Opts) of
        {ok, Status, _, Ref} when Status =:= 200 orelse Status =:= 201 ->
            Res = couchbeam_httpc:json_body(Ref),
            {ok, Res};
        {ok, Status, Headers, Ref} ->
            {ok, Body} = hackney:body(Ref),
            {error, {bad_response, {Status, Headers, Body}}};
        Error ->
            Error
    end.

%% @doc Handle replication.
%% @spec replicate(Server::server(), Source::string(), Target::target())
%%          ->  {ok, Result}|{error, Error}
replicate(Server, Source, Target) ->
    replicate(Server, Source, Target, []).

%% @doc handle Replication. Allows to pass options with source and
%% target.  Options is a Json object.
%% ex:
%% ```
%% Options = [{<<"create_target">>, true}]}
%% couchbeam:replicate(S, "testdb", "testdb2", Options).
%% '''
replicate(Server, Source, Target, {Props}) ->
    replicate(Server, Source, Target, Props);
replicate(Server, #db{name=Source}, Target, Options) ->
    replicate(Server, Source, Target, Options);
replicate(Server, Source, #db{name=Target}, Options) ->
    replicate(Server, Source, Target, Options);
replicate(Server, #db{name=Source}, #db{name=Target}, Options) ->
    replicate(Server, Source, Target, Options);
replicate(Server, Source, Target, Options) ->
    RepProp = [
        {<<"source">>, couchbeam_util:to_binary(Source)},
        {<<"target">>, couchbeam_util:to_binary(Target)} | Options
    ],
    replicate(Server, {RepProp}).

%% @doc get list of databases on a CouchDB node
%% @spec all_dbs(server()) -> {ok, iolist()}
all_dbs(#server{}=Server) -> all_dbs(Server, []).

%% @doc get list of databases on a CouchDB node with optional filter
%% @spec all_dbs(server(), view_options()) -> {ok, iolist()}
all_dbs(#server{url=ServerUrl, options=Opts}, Options) ->
    Args = couchbeam_view:parse_view_options(Options),
    Url = hackney_url:make_url(ServerUrl, <<"_all_dbs">>, Args#view_query_args.options),
    Resp = couchbeam_httpc:db_request(get, Url, [], <<>>, Opts, [200]),
    case Resp of
        {ok, _, _, Ref} ->
            AllDbs = couchbeam_httpc:json_body(Ref),
            {ok, AllDbs};
        Error ->
            Error
    end.

design_info(#db{server=Server, name=DbName, options=Opts}, DesignName) ->
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server),
                               [DbName, <<"_design">>, DesignName, <<"_info">>],
                               []),
    Resp = couchbeam_httpc:db_request(get, Url, [], <<>>, Opts, [200]),
    case Resp of
        {ok, _, _, Ref} ->
            DesignInfo = couchbeam_httpc:json_body(Ref),
            {ok, DesignInfo};
        Error ->
            Error
    end.

view_cleanup(#db{server=Server, name=DbName, options=Opts}) ->
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server),
                               [DbName, <<"_view_cleanup">>],
                               []),
    Resp = couchbeam_httpc:db_request(post, Url, [], <<>>, Opts, [200]),
    case Resp of
        {ok, _, _, Ref} ->
            catch hackney:skip_body(Ref),
            ok;
        Error ->
            Error
    end.

%% @doc test if db with dbname exists on the CouchDB node
%% @spec db_exists(server(), string()) -> boolean()
db_exists(#server{url=ServerUrl, options=Opts}, DbName) ->
    Url = hackney_url:make_url(ServerUrl, couchbeam_util:dbname(DbName), []),
    case couchbeam_httpc:db_request(head, Url, [], <<>>, Opts, [200]) of
        {ok, 200, _}->
            true;
        _Error ->
            false
    end.

%% @doc Create a database and a client for connectiong to it.
%% @equiv create_db(Server, DbName, [], [])
create_db(Server, DbName) ->
    create_db(Server, DbName, [], []).

%% @doc Create a database and a client for connectiong to it.
%% @equiv create_db(Server, DbName, Options, [])
create_db(Server, DbName, Options) ->
    create_db(Server, DbName, Options, []).

%% @doc Create a database and a client for connectiong to it.
%%
%%      Connections are made to:
%%      ```http://Host:PortPrefix/DbName'''
%%
%% If ssl is set https is used. See server_connections for options.
%% Params is a list of optionnal query argument you want to pass to the
%% db. Useful for bigcouch for example.
%%
%% @spec create_db(Server::server(), DbName::string(),
%%                 Options::optionList(), Params::list()) -> {ok, db()|{error, Error}}
create_db(#server{url=ServerUrl, options=Opts}=Server, DbName0, Options,
          Params) ->
    DbName = couchbeam_util:dbname(DbName0),
    Options1 = couchbeam_util:propmerge1(Options, Opts),
    Url = hackney_url:make_url(ServerUrl, DbName, Params),
    Resp = couchbeam_httpc:db_request(put, Url, [], <<>>, Options1, [201]),
    case Resp of
        {ok, _Status, _Headers, Ref} ->
            hackney:skip_body(Ref),
            {ok, #db{server=Server, name=DbName, options=Options1}};
        {error, precondition_failed} ->
            {error, db_exists};
       Error ->
          Error
    end.

%% @doc Create a client for connection to a database
%% @equiv open_db(Server, DbName, [])
open_db(Server, DbName) ->
    open_db(Server, DbName, []).

%% @doc Create a client for connection to a database
%% @spec open_db(Server::server(), DbName::string(), Options::optionList())
%%              -> {ok, db()}
open_db(#server{options=Opts}=Server, DbName, Options) ->
    Options1 = couchbeam_util:propmerge1(Options, Opts),
    {ok, #db{server=Server, name=couchbeam_util:dbname(DbName), options=Options1}}.


%% @doc Create a client for connecting to a database and create the
%%      database if needed.
%% @equiv open_or_create_db(Server, DbName, [], [])
open_or_create_db(Server, DbName) ->
    open_or_create_db(Server, DbName, [], []).

%% @doc Create a client for connecting to a database and create the
%%      database if needed.
%% @equiv open_or_create_db(Server, DbName, Options, [])
open_or_create_db(Server, DbName, Options) ->
    open_or_create_db(Server, DbName, Options, []).

%% @doc Create a client for connecting to a database and create the
%%      database if needed.
%% @spec open_or_create_db(server(), string(), list(), list()) -> {ok, db()|{error, Error}}
open_or_create_db(#server{url=ServerUrl, options=Opts}=Server, DbName0,
                  Options, Params) ->

    DbName = couchbeam_util:dbname(DbName0),
    Url = hackney_url:make_url(ServerUrl, DbName, []),
    Opts1 = couchbeam_util:propmerge1(Options, Opts),
    Resp = couchbeam_httpc:request(get, Url, [], <<>>, Opts1),
    case couchbeam_httpc:db_resp(Resp, [200]) of
        {ok, _Status, _Headers, Ref} ->
            hackney:skip_body(Ref),
            open_db(Server, DbName, Options);
        {error, not_found} ->
            create_db(Server, DbName, Options, Params);
        Error ->
            Error
    end.

%% @doc delete database
%% @equiv delete_db(Server, DbName)
delete_db(#db{server=Server, name=DbName}) ->
    delete_db(Server, DbName).

%% @doc delete database
%% @spec delete_db(server(), DbName) -> {ok, iolist()|{error, Error}}
delete_db(#server{url=ServerUrl, options=Opts}, DbName) ->
    Url = hackney_url:make_url(ServerUrl, couchbeam_util:dbname(DbName), []),
    Resp = couchbeam_httpc:request(delete, Url, [], <<>>, Opts),
    case couchbeam_httpc:db_resp(Resp, [200]) of
        {ok, _, _, Ref} ->
            {ok, couchbeam_httpc:json_body(Ref)};
        Error ->
            Error
    end.

%% @doc get database info
%% @spec db_info(db()) -> {ok, iolist()|{error, Error}}
db_info(#db{server=Server, name=DbName, options=Opts}) ->
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server), couchbeam_util:dbname(DbName), []),
    case couchbeam_httpc:db_request(get, Url, [], <<>>, Opts, [200]) of
        {ok, _Status, _Headers, Ref} ->
            Infos = couchbeam_httpc:json_body(Ref),
            {ok, Infos};
        {error, not_found} ->
            {error, db_not_found};
       Error ->
          Error
    end.

%% @doc test if doc with uuid exists in the given db
%% @spec doc_exists(db(), string()) -> boolean()
doc_exists(#db{server=Server, options=Opts}=Db, DocId) ->
    DocId1 = couchbeam_util:encode_docid(DocId),
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server), couchbeam_httpc:doc_url(Db, DocId1), []),
    case couchbeam_httpc:db_request(head, Url, [], <<>>, Opts, [200]) of
        {ok, _, _} -> true;
        _Error -> false
    end.

%% @doc open a document
%% @equiv open_doc(Db, DocId, [])
open_doc(Db, DocId) ->
    open_doc(Db, DocId, []).

%% @doc open a document
%% Params is a list of query argument. Have a look in CouchDb API
%% @spec open_doc(Db::db(), DocId::string(), Params::list())
%%          -> {ok, Doc}|{error, Error}
open_doc(#db{server=Server, options=Opts}=Db, DocId, Params) ->
    DocId1 = couchbeam_util:encode_docid(DocId),

    %% is there any accepted content-type passed to the params?
    {Accept, Params1} = case proplists:get_value(accept, Params) of
        unefined -> {any, Params};
        A -> {A, proplists:delete(accept, Params)}
    end,
    %% set the headers with the accepted content-type if needed
    Headers = case {Accept, proplists:get_value("attachments", Params)} of
        {any, true} ->
            %% only use the more efficient method when we get the
            %% attachments so we don't use much bandwidth.
            [{<<"Accept">>, <<"multipart/related">>}];
        {Accept, _} when is_binary(Accept) ->
            %% accepted content-type has been forced
            [{<<"Accept">>, Accept}];
        _ ->
            []
    end,
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server), couchbeam_httpc:doc_url(Db, DocId1),
                               Params1),
    case couchbeam_httpc:db_request(get, Url, Headers, <<>>, Opts,
                                    [200, 201]) of
        {ok, _, RespHeaders, Ref} ->
            case hackney_headers:parse(<<"content-type">>, RespHeaders) of
                {<<"multipart">>, _, _} ->
                    %% we get a multipart request, start to parse it.
                    InitialState =  {Ref, fun() ->
                                    couchbeam_httpc:wait_mp_doc(Ref, <<>>)
                            end},
                    {ok, {multipart, InitialState}};
                _ ->
                    {ok, couchbeam_httpc:json_body(Ref)}
            end;
        Error ->
            Error
    end.

%% @doc stream the multipart response of the doc API. Use this function
%% when you get `{ok, {multipart, State}}' from the function
%% `couchbeam:open_doc/3'.
-spec stream_doc(doc_stream()) ->
    {doc, doc()}
    | {att, Name :: binary(), doc_stream()}
    | {att_body, Name :: binary(), Chunk :: binary(), doc_stream()}
    | {att_eof, Name :: binary(), doc_stream()}
    | eof
    | {error, term()}.
stream_doc({_Ref, Cont}) ->
    Cont().

%% @doc stop to receive the multipart response of the doc api and close
%% the connection.
-spec end_doc_stream(doc_stream()) -> ok.
end_doc_stream({Ref, _Cont}) ->
    hackney:close(Ref).

%% @doc save a document
%% @equiv save_doc(Db, Doc, [])
save_doc(Db, Doc) ->
    save_doc(Db, Doc, []).

%% @doc save a *document
%% A document is a Json object like this one:
%%
%%      ```{[
%%          {<<"_id">>, <<"myid">>},
%%          {<<"title">>, <<"test">>}
%%      ]}'''
%%
%% Options are arguments passed to the request. This function return a
%% new document with last revision and a docid. If _id isn't specified in
%% document it will be created. Id is created by extracting an uuid from
%% the couchdb node.
%%
%% @spec save_doc(Db::db(), Doc, Options::list()) -> {ok, Doc1}|{error, Error}
save_doc(Db, Doc, Options) ->
    save_doc(Db, Doc, [], Options).


%% @doc save a *document with all its attacjments
%% A document is a Json object like this one:
%%
%%      ```{[
%%          {<<"_id">>, <<"myid">>},
%%          {<<"title">>, <<"test">>}
%%      ]}'''
%%
%% Options are arguments passed to the request. This function return a
%% new document with last revision and a docid. If _id isn't specified in
%% document it will be created. Id is created by extracting an uuid from
%% the couchdb node.
%%
%% If the attachments is not empty, the doc will be sent as multipart.
%% Attachments are passed as a list of the following tuples:
%%
%% - `{Name :: binary(), Bin :: binary()}'
%% - `{Name :: binary(), Bin :: binary(), Encoding :: binary()}'
%% - `{ Name :: binary(), Bin :: binary(), Type :: binary(), Encoding :: binary()}'
%% - `{ Name :: binary(), {file, Path ::  string()}}'
%% - `{ Name :: binary(), {file, Path ::  string()}, Encoding :: binary()}'
%% - `{ Name :: binary(), Fun :: fun(), Length :: integer()}'
%% - `{ Name :: binary(), Fun :: fun(), Length :: integer(), Encoding :: binary()}'
%% - `{Name :: binary(), Fun :: fun(), Length :: integer(), Type :: binary(), Encoding :: binary()}'
%% - `{ Name :: binary(), {Fun :: fun(), Acc :: any()}, Length :: integer()}'
%% - `{ Name :: binary(), {Fun :: fun(), Acc :: any()}, Length :: integer(), Encoding :: binary()}'
%% - `{ Name :: binary(), {Fun :: fun(), Acc :: any()}, Length :: integer(), Type :: binary(), Encoding :: binary()}.'
%%
%% where `Type` is the content-type of the attachments (detected in other
%% case) and `Encoding` the encoding of the attachments:
%% `<<"identity">>' if normal or `<<"gzip">>' if the attachments is
%% gzipped.

-spec save_doc(Db::db(), doc(), mp_attachments(), Options::list()) ->
    {ok, doc()} | {error, term()}.
save_doc(#db{server=Server, options=Opts}=Db, {Props}=Doc, Atts, Options) ->
    DocId = case couchbeam_util:get_value(<<"_id">>, Props) of
        undefined ->
            [Id] = get_uuid(Server),
            Id;
        DocId1 ->
            couchbeam_util:encode_docid(DocId1)
    end,
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server), couchbeam_httpc:doc_url(Db, DocId),
                               Options),
    case Atts of
        [] ->
            JsonDoc = couchbeam_ejson:encode(Doc),
            Headers = [{<<"Content-Type">>, <<"application/json">>}],
            case couchbeam_httpc:db_request(put, Url, Headers, JsonDoc, Opts,
                                    [200, 201, 202]) of
                {ok, _, _, Ref} ->
                    {JsonProp} = couchbeam_httpc:json_body(Ref),
                    NewRev = couchbeam_util:get_value(<<"rev">>, JsonProp),
                    NewDocId = couchbeam_util:get_value(<<"id">>, JsonProp),
                    Doc1 = couchbeam_doc:set_value(<<"_rev">>, NewRev,
                        couchbeam_doc:set_value(<<"_id">>, NewDocId, Doc)),
                    {ok, Doc1};
                Error ->
                    Error
            end;
        _ ->
            Boundary = couchbeam_uuids:random(),

            %% for now couchdb can't received chunked multipart stream
            %% so we have to calculate the content-length. It also means
            %% that we need to know the size of each attachments. (Which
            %% should be expected).
            {CLen, JsonDoc, Doc2} = couchbeam_httpc:len_doc_to_mp_stream(Atts, Boundary, Doc),
            CType = <<"multipart/related; boundary=\"",
                      Boundary/binary, "\"" >>,

            Headers = [{<<"Content-Type">>, CType},
                       {<<"Content-Length">>, hackney_bstr:to_binary(CLen)}],

            case couchbeam_httpc:request(put, Url, Headers, stream,
                                         Opts) of
                {ok, Ref} ->
                    couchbeam_httpc:send_mp_doc(Atts, Ref, Boundary, JsonDoc, Doc2);
                Error ->
                    Error
            end
    end.

%% @doc delete a document
%% @equiv delete_doc(Db, Doc, [])
delete_doc(Db, Doc) ->
    delete_doc(Db, Doc, []).

%% @doc delete a document
%% if you want to make sure the doc it emptied on delete, use the option
%% {empty_on_delete,  true} or pass a doc with just _id and _rev
%% members.
%% @spec delete_doc(Db, Doc, Options) -> {ok,Result}|{error,Error}
delete_doc(Db, Doc, Options) ->
     delete_docs(Db, [Doc], Options).

%% @doc delete a list of documents
%% @equiv delete_docs(Db, Docs, [])
delete_docs(Db, Docs) ->
    delete_docs(Db, Docs, []).

%% @doc delete a list of documents
%% if you want to make sure the doc it emptied on delete, use the option
%% {empty_on_delete,  true} or pass a doc with just _id and _rev
%% members.
%% @spec delete_docs(Db::db(), Docs::list(),Options::list()) -> {ok, Result}|{error, Error}
delete_docs(Db, Docs, Options) ->
    Empty = couchbeam_util:get_value("empty_on_delete", Options, false),

    {FinalDocs, FinalOptions} = case Empty of
        true ->
            Docs1 = lists:map(fun(Doc)->
                        {[{<<"_id">>, couchbeam_doc:get_id(Doc)},
                         {<<"_rev">>, couchbeam_doc:get_rev(Doc)},
                         {<<"_deleted">>, true}]}
                 end, Docs),
             {Docs1, proplists:delete("all_or_nothing", Options)};
         _ ->
            Docs1 = lists:map(fun({DocProps})->
                        {[{<<"_deleted">>, true}|DocProps]}
                end, Docs),
            {Docs1, Options}
    end,
    save_docs(Db, FinalDocs, FinalOptions).

%% @doc save a list of documents
%% @equiv save_docs(Db, Docs, [])
save_docs(Db, Docs) ->
    save_docs(Db, Docs, []).

%% @doc save a list of documents
%% @spec save_docs(Db::db(), Docs::list(),Options::list()) -> {ok, Result}|{error, Error}
save_docs(#db{server=Server, options=Opts}=Db, Docs, Options) ->
    Docs1 = [maybe_docid(Server, Doc) || Doc <- Docs],
    Options1 = couchbeam_util:parse_options(Options),
    DocOptions = [
        {list_to_binary(K), V} || {K, V} <- Options1,
        (K =:= "all_or_nothing" orelse K =:= "new_edits") andalso is_boolean(V)
    ],
    Options2 = [
        {K, V} || {K, V} <- Options1,
        K =/= "all_or_nothing" andalso K =/= "new_edits"
    ],
    Body = couchbeam_ejson:encode({[{<<"docs">>, Docs1}|DocOptions]}),
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server),
                               [couchbeam_httpc:db_url(Db), <<"_bulk_docs">>],
                               Options2),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case couchbeam_httpc:db_request(post, Url, Headers, Body, Opts, [201]) of
        {ok, _, _, Ref} ->
            {ok, couchbeam_httpc:json_body(Ref)};
        Error ->
            Error
        end.

%% @doc duplicate a document using the doc API
copy_doc(#db{server=Server}=Db, Doc) ->
    [DocId] = get_uuid(Server),
    copy_doc(Db, Doc, DocId).

%% @doc copy a doc to a destination. If the destination exist it will
%% use the last revision, in other case a new doc is created with the
%% the current doc revision.
copy_doc(Db, Doc, Dest) when is_binary(Dest) ->
    Destination = case open_doc(Db, Dest) of
        {ok, DestDoc} ->
            Rev = couchbeam_doc:get_rev(DestDoc),
            {Dest, Rev};
        _ ->
            {Dest, <<>>}
    end,
    do_copy(Db, Doc, Destination);
copy_doc(Db, Doc, {Props}) ->
    DocId = proplists:get_value(<<"_id">>, Props),
    Rev = proplists:get_value(<<"_rev">>, Props, <<>>),
    do_copy(Db, Doc, {DocId, Rev}).

do_copy(Db, {Props}, Destination) ->
    case proplists:get_value(<<"_id">>, Props) of
        undefined ->
            {error, invalid_source};
        DocId ->
            DocRev = proplists:get_value(<<"_rev">>, Props, nil),
            do_copy(Db, {DocId, DocRev}, Destination)
    end;
do_copy(Db, DocId, Destination) when is_binary(DocId) ->
    do_copy(Db, {DocId, nil}, Destination);
do_copy(#db{server=Server, options=Opts}=Db, {DocId, DocRev},
        {DestId, DestRev}) ->

    Destination = case DestRev of
        <<>> -> DestId;
        _ -> << DestId/binary, "?rev=", DestRev/binary >>
    end,
    Headers = [{<<"Destination">>, Destination}],
    {Headers1, Params} = case {DocRev, DestRev} of
        {nil, _} ->
            {Headers, []};
        {_, <<>>} ->
            {[{<<"If-Match">>, DocRev} | Headers], []};
        {_, _} ->
            {Headers, [{<<"rev">>, DocRev}]}
    end,

    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server), couchbeam_httpc:doc_url(Db, DocId),
                               Params),

    case couchbeam_httpc:db_request(copy, Url, Headers1, <<>>,
                                    Opts, [201]) of
        {ok, _, _, Ref} ->
            {JsonProp} = couchbeam_httpc:json_body(Ref),
            NewRev = couchbeam_util:get_value(<<"rev">>, JsonProp),
            NewDocId = couchbeam_util:get_value(<<"id">>, JsonProp),
            {ok, NewDocId, NewRev};
        Error ->
            Error
    end.

%% @doc get the last revision of the document
lookup_doc_rev(Db, DocId) ->
    lookup_doc_rev(Db, DocId, []).

lookup_doc_rev(#db{server=Server, options=Opts}=Db, DocId, Params) ->
    DocId1 = couchbeam_util:encode_docid(DocId),
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server), couchbeam_httpc:doc_url(Db, DocId1),
                               Params),
    case couchbeam_httpc:db_request(head, Url, [], <<>>, Opts, [200]) of
        {ok, _, Headers} ->
            HeadersDict = hackney_headers:new(Headers),
            re:replace(hackney_headers:get_value(<<"etag">>, HeadersDict),
                <<"\"">>, <<>>, [global, {return, binary}]);
        Error ->
            Error
    end.

%% @doc fetch a document attachment
%% @equiv fetch_attachment(Db, DocId, Name, [])
fetch_attachment(Db, DocId, Name) ->
    fetch_attachment(Db, DocId, Name, []).

%% @doc fetch a document attachment
%% Options are
%% <ul>
%% <li>`stream': to start streaming an attachment. the function return
%% `{ok, Ref}' where is a ref to the attachment</li>
%% <li>Other options that can be sent using the REST API</li>
%% </ul>
%%
-spec fetch_attachment(db(), string(), string(),
                       list())
    -> {ok, binary()}| {ok, atom()} |{error, term()}.
fetch_attachment(#db{server=Server, options=Opts}=Db, DocId, Name, Options0) ->
    {Stream, Options} = case couchbeam_util:get_value(stream, Options0) of
        undefined ->
            {false, Options0};
        true ->
            {true, proplists:delete(stream, Options0)};
        _ ->
            {false, proplists:delete(stream, Options0)}
    end,


    Options1 = couchbeam_util:parse_options(Options),

    %% custom headers. Allows us to manage Range.
    {Options2, Headers} = case couchbeam_util:get_value("headers", Options1) of
        undefined ->
            {Options1, []};
        Headers1 ->
            {proplists:delete("headers", Options1), Headers1}
    end,

    DocId1 = couchbeam_util:encode_docid(DocId),
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server),
                               [couchbeam_httpc:db_url(Db), DocId1,
                                Name],
                               Options2),
    case hackney:get(Url, Headers, <<>>, Opts) of
        {ok, 200, _, Ref} when Stream /= true ->
            hackney:body(Ref);
        {ok, 200, _, Ref} ->
            {ok, Ref};
        {ok, 404, _, Ref} ->
            hackney:skip_body(Ref),
            {error, not_found};
        {ok, Status, Headers, Ref} ->
            {ok, Body} = hackney:body(Ref),
            {error, {bad_response, {Status, Headers, Body}}};

        Error ->
            Error
    end.

%% @doc fetch an attachment chunk.
%% Use this function when you pass the `stream' option to the
%% `couchbeam:fetch_attachment/4' function.
%% This function return the following response:
%%      <dl>
%%          <dt>done</dt>
%%              <dd>You got all the attachment</dd>
%%          <dt>{ok, binary()}</dt>
%%              <dd>Part of the attachment</dd>
%%          <dt>{error, term()}</dt>
%%              <dd>n error occurred</dd>
%%      </dl>
%%

-spec stream_attachment(atom()) -> {ok, binary()}
    | done
    | {error, term()}.
stream_attachment(Ref) ->
    hackney:stream_body(Ref).

%% @doc put an attachment
%% @equiv put_attachment(Db, DocId, Name, Body, [])
put_attachment(Db, DocId, Name, Body)->
    put_attachment(Db, DocId, Name, Body, []).

%% @doc put an attachment
%% @spec put_attachment(Db::db(), DocId::string(), Name::string(),
%%                      Body::body(), Option::optionList()) -> {ok, iolist()}
%%       optionList() = [option()]
%%       option() = {rev, string()} |
%%                  {content_type, string()} |
%%                  {content_length, string()}
%%       body() = [] | string() | binary() | fun_arity_0() |
%%       {fun_arity_1(), initial_state(), stream}
%%       initial_state() = term()
put_attachment(#db{server=Server, options=Opts}=Db, DocId, Name, Body,
               Options) ->
    QueryArgs = case couchbeam_util:get_value(rev, Options) of
        undefined -> [];
        Rev -> [{<<"rev">>, couchbeam_util:to_binary(Rev)}]
    end,

    Headers = couchbeam_util:get_value(headers, Options, []),


    FinalHeaders = lists:foldl(fun(Option, Acc) ->
                case Option of
                        {content_length, V} ->
                            V1 = couchbeam_util:to_binary(V),
                            [{<<"Content-Length">>, V1}|Acc];
                        {content_type, V} ->
                            V1 = couchbeam_util:to_binary(V),
                            [{<<"Content-Type">>, V1}|Acc];
                        _ ->
                            Acc
                end
        end, Headers, Options),

    DocId1 = couchbeam_util:encode_docid(DocId),
    AttName = couchbeam_util:encode_att_name(Name),
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server), [couchbeam_httpc:db_url(Db), DocId1,
                                                    AttName],
                               QueryArgs),

    case couchbeam_httpc:db_request(put, Url, FinalHeaders, Body, Opts,
                                   [201]) of
        {ok, _, _, Ref} ->
            JsonBody = couchbeam_httpc:json_body(Ref),
            {[{<<"ok">>, true}|R]} = JsonBody,
            {ok, {R}};
        {ok, Ref} ->
            {ok, Ref};
        Error ->
            Error
    end.

%% @doc send an attachment chunk
%% Msg could be Data, eof to stop sending.
send_attachment(Ref, eof) ->
    case hackney:finish_send_body(Ref) of
        ok ->
            Resp =  hackney:start_response(Ref),
            couchbeam_httpc:reply_att(Resp);
        Error ->
            Error
    end;
send_attachment(Ref, Msg) ->
    Reply = hackney:send_body(Ref, Msg),
    couchbeam_httpc:reply_att(Reply).


%% @doc delete a document attachment
%% @equiv delete_attachment(Db, Doc, Name, [])
delete_attachment(Db, Doc, Name) ->
    delete_attachment(Db, Doc, Name, []).

%% @doc delete a document attachment
%% @spec(db(), string()|list(), string(), list() -> {ok, Result} | {error, Error}
delete_attachment(#db{server=Server, options=Opts}=Db, DocOrDocId, Name,
                  Options) ->
    Options1 = couchbeam_util:parse_options(Options),
    {Rev, DocId} = case DocOrDocId of
        {Props} ->
            Rev1 = couchbeam_util:get_value(<<"_rev">>, Props),
            DocId1 = couchbeam_util:get_value(<<"_id">>, Props),
            {Rev1, DocId1};
        DocId1 ->
            Rev1 = couchbeam_util:get_value("rev", Options1),
            {Rev1, DocId1}
    end,
    case Rev of
        undefined ->
           {error, rev_undefined};
        _ ->
            Options2 = case couchbeam_util:get_value("rev", Options1) of
                undefined ->
                    [{<<"rev">>, couchbeam_util:to_binary(Rev)}|Options1];
                _ ->
                    Options1
            end,
            Url = hackney_url:make_url(couchbeam_httpc:server_url(Server), [couchbeam_httpc:db_url(Db),
                                                            DocId,
                                                            Name],
                                       Options2),

            case couchbeam_httpc:db_request(delete, Url, [], <<>>, Opts,
                                            [200]) of
            {ok, _, _, Ref} ->
                {[{<<"ok">>,true}|R]} = couchbeam_httpc:json_body(Ref),
                {ok, {R}};
            Error ->
                Error
            end
    end.

%% @doc commit all docs in memory
%% @equiv ensure_full_commit(Db, [])
ensure_full_commit(Db) ->
    ensure_full_commit(Db, []).

%% @doc commit all docs in memory
-spec ensure_full_commit(Db::db(), Options::list())
    -> {ok, InstancestartTime :: binary()}
    | {error, term()}.
ensure_full_commit(#db{server=Server, options=Opts}=Db, Options) ->
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server), [couchbeam_httpc:db_url(Db),
                                                    <<"_ensure_full_commit">>],
                               Options),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case couchbeam_httpc:db_request(post, Url, Headers, <<>>, Opts, [201]) of
        {ok, _, _, Ref} ->
            {Props} = couchbeam_httpc:json_body(Ref),
            {ok, proplists:get_value(<<"instance_start_time">>, Props)};
        Error ->
            Error
    end.

%% @doc Compaction compresses the database file by removing unused
%% sections created during updates.
%% See [http://wiki.apache.org/couchdb/Compaction] for more informations
%% @spec compact(Db::db()) -> ok|{error, term()}
compact(#db{server=Server, options=Opts}=Db) ->
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server), [couchbeam_httpc:db_url(Db),
                                                    <<"_compact">>],
                               []),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case couchbeam_httpc:db_request(post, Url, Headers, <<>>, Opts, [202]) of
        {ok, _, _, Ref} ->
            hackney:skip_body(Ref),
            ok;
        Error ->
            Error
    end.
%% @doc Like compact/1 but this compacts the view index from the
%% current version of the design document.
%% See [http://wiki.apache.org/couchdb/Compaction#View_compaction] for more informations
%% @spec compact(Db::db(), ViewName::string()) -> ok|{error, term()}
compact(#db{server=Server, options=Opts}=Db, DesignName) ->
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server), [couchbeam_httpc:db_url(Db),
                                                   <<"_compact">>,
                                                   DesignName], []),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case couchbeam_httpc:db_request(post, Url, Headers, <<>>, Opts, [202]) of
        {ok, _, _, Ref} ->
            hackney:skip_body(Ref),
            ok;
        Error ->
            Error
    end.


%% @doc get missing revisions
-spec get_missing_revs(#db{}, [{binary(), [binary()]}]) ->
    {ok, [{DocId :: binary(), [MissingRev :: binary()], [
                    PossibleAncestor :: binary()]}]}
    | {error, term()}.
get_missing_revs(#db{server=Server, options=Opts}=Db, IdRevs) ->
    Json = couchbeam_ejson:encode({IdRevs}),
    Url = hackney_url:make_url(couchbeam_httpc:server_url(Server), [couchbeam_httpc:db_url(Db),
                                                   <<"_revs_diff">>],
                              []),

    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case couchbeam_httpc:db_request(post, Url, Headers, Json, Opts,
                                    [200]) of
        {ok, _, _, Ref} ->
            {Props} = couchbeam_httpc:json_body(Ref),
            Res = lists:map(fun({Id, {Result}}) ->
                            MissingRevs = proplists:get_value(
                                    <<"missing">>, Result
                            ),
                            PossibleAncestors = proplists:get_value(
                                <<"possible_ancestors">>, Result, []
                            ),
                            {Id, MissingRevs, PossibleAncestors}
                    end, Props),
            {ok, Res};
        Error ->
            Error
    end.

%% --------------------------------------------------------------------
%% private functions.
%% --------------------------------------------------------------------

%% add missing docid to a list of documents if needed
maybe_docid(Server, {DocProps}) ->
    case couchbeam_util:get_value(<<"_id">>, DocProps) of
        undefined ->
            [DocId] = get_uuid(Server),
            {[{<<"_id">>, DocId}|DocProps]};
        _DocId ->
            {DocProps}
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").


clean_dbs() ->
    Server = couchbeam:server_connection(),
    catch couchbeam:delete_db(Server, "couchbeam_testdb"),
    catch couchbeam:delete_db(Server, "couchbeam_testdb2"),
    catch couchbeam:delete_db(Server, "couchbeam_testdb3"),
    timer:sleep(300),
    ok.

start_couchbeam_tests() ->
    {ok, _} = application:ensure_all_started(couchbeam),
    clean_dbs().

basic_test() ->
    start_couchbeam_tests(),
    Server = couchbeam:server_connection(),
    {ok, {Data}} = couchbeam:server_info(Server),
    ?assertEqual(<<"Welcome">>, proplists:get_value(<<"couchdb">>, Data)),
    ok.

db_test() ->
    start_couchbeam_tests(),
    Server = couchbeam:server_connection(),

    %% test db creation
    ?assertMatch({ok, _}, couchbeam:create_db(Server, "couchbeam_testdb")),
    ?assertEqual({error, db_exists}, couchbeam:create_db(Server, "couchbeam_testdb")),
    ?assertMatch({ok, _}, couchbeam:create_db(Server, "couchbeam_testdb2")),

    {ok, AllDbs} = couchbeam:all_dbs(Server),
    ?assert(is_list(AllDbs) =:= true),
    ?assertEqual(true, lists:member(<<"couchbeam_testdb">>, AllDbs)),
    ?assertEqual(true, lists:member(<<"couchbeam_testdb2">>, AllDbs)),
    ?assertEqual(true, couchbeam:db_exists(Server, "couchbeam_testdb")),
    ?assertEqual(true, couchbeam:db_exists(Server, "couchbeam_testdb2")),
    ?assertMatch({ok, _}, couchbeam:delete_db(Server, "couchbeam_testdb2")),
    {ok, AllDbs1} = couchbeam:all_dbs(Server),
    ?assertEqual(true, lists:member(<<"couchbeam_testdb">>, AllDbs1)),
    ?assertEqual(false, lists:member(<<"couchbeam_testdb2">>, AllDbs1)),
    ?assertEqual(true, couchbeam:db_exists(Server, "couchbeam_testdb")),
    ?assertEqual(false, couchbeam:db_exists(Server, "couchbeam_testdb2")),

    ?assertMatch({ok, _}, couchbeam:open_or_create_db(Server, "couchbeam_testdb")),
    ?assertMatch({ok, _}, couchbeam:open_or_create_db(Server, "couchbeam_testdb2")),
    ok.

basic_doc_test() ->
    start_couchbeam_tests(),
    Server = couchbeam:server_connection(),
    {ok, Db} = couchbeam:create_db(Server, "couchbeam_testdb"),
    {ok, Doc} = couchbeam:save_doc(Db, {[{<<"test">>, <<"blah">>}]}),
    ?assertMatch({_}, Doc),
    {ok, {Props}} = couchbeam:save_doc(Db, {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]}),
    ?assertEqual(<<"test">>, proplists:get_value(<<"_id">>, Props)),
    ?assertEqual({error, conflict}, couchbeam:save_doc(Db, {[{<<"_id">>,<<"test">>}, {<<"test">>,<<"blah">>}]})),

    Rev = couchbeam:lookup_doc_rev(Db, "test"),
    {ok, {Doc1}} = couchbeam:open_doc(Db, <<"test">>),
    ?assertEqual(Rev, proplists:get_value(<<"_rev">>, Doc1)),
    ?assertEqual(<<"blah">>, proplists:get_value(<<"test">>, Doc1)),

    _ = couchbeam:save_doc(Db, {[{<<"_id">>,<<"test2">>}, {<<"test">>,<<"blah">>}]}),
    {ok, Doc2} = couchbeam:open_doc(Db, "test2"),
    ?assertMatch({_}, Doc2),
    ?assertEqual(true, couchbeam_doc:is_saved(Doc2)),
    ?assertEqual(<<"test2">>, couchbeam_doc:get_id(Doc2)),
    ?assertMatch(true, couchbeam:doc_exists(Db, "test2")),
    ?assertMatch({ok, _}, couchbeam:delete_doc(Db, Doc2)),
    ?assertEqual({error, not_found}, couchbeam:open_doc(Db, "test2")),
    ?assertMatch(false, couchbeam:doc_exists(Db, "test2")),

    Doc3 = {[{<<"_id">>, <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>}]},
    {ok, _Doc4} = couchbeam:save_doc(Db, Doc3),
    {ok, Doc5} = couchbeam:open_doc(Db, <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>),
    ?assertEqual( <<"~!@#$%^&*()_+-=[]{}|;':,./<> ?">>, couchbeam_doc:get_id(Doc5)),
    ok.

bulk_doc_test() ->
    start_couchbeam_tests(),
    Server = couchbeam:server_connection(),
    {ok, Db} = couchbeam:create_db(Server, "couchbeam_testdb"),

    Doc1 = {[{<<"_id">>, <<"a">>}]},
    Doc2 = {[{<<"_id">>, <<"b">>}]},
    {ok, [{Props1}, {Props2}]} = couchbeam:save_docs(Db, [Doc1, Doc2]),
    ?assertEqual(<<"a">>, proplists:get_value(<<"id">>, Props1)),
    ?assertEqual(<<"b">>, proplists:get_value(<<"id">>, Props2)),
    ?assertMatch(true, couchbeam:doc_exists(Db, "a")),
    ?assertMatch(true, couchbeam:doc_exists(Db, "b")),


    {ok, Doc3} = couchbeam:open_doc(Db, <<"a">>),
    {ok, Doc4} = couchbeam:open_doc(Db, <<"b">>),
    couchbeam:delete_docs(Db, [Doc3, Doc4]),
    ?assertEqual({error, not_found}, couchbeam:open_doc(Db, <<"a">>)),
    ok.

copy_doc_test() ->
    start_couchbeam_tests(),
    Server = couchbeam:server_connection(),
    {ok, Db} = couchbeam:create_db(Server, "couchbeam_testdb"),

    {ok, Doc} = couchbeam:save_doc(Db, {[{<<"test">>, 1}]}),
    {ok, CopyId, _Rev} = couchbeam:copy_doc(Db, Doc),
    {ok, Doc2} = couchbeam:open_doc(Db, CopyId),
    ?assertEqual(1, couchbeam_doc:get_value(<<"test">>, Doc2)),

    {ok, _Doc3} = couchbeam:save_doc(Db, {[{<<"_id">>, <<"test_copy">>}]}),
    {ok, CopyId1, _} = couchbeam:copy_doc(Db, Doc, <<"test_copy">>),
    ?assertEqual(<<"test_copy">>, CopyId1),
    {ok, Doc4} = couchbeam:open_doc(Db, CopyId1),
    ?assertEqual(1, couchbeam_doc:get_value(<<"test">>, Doc4)),
    ok.


attachments_test() ->
    start_couchbeam_tests(),
    Server = couchbeam:server_connection(),
    {ok, Db} = couchbeam:create_db(Server, "couchbeam_testdb"),
    Doc = {[{<<"_id">>, <<"test">>}]},
    {ok, Doc1} = couchbeam:save_doc(Db, Doc),
    RevDoc1 = couchbeam_doc:get_rev(Doc1),
    {ok, {Res}} = couchbeam:put_attachment(Db,"test", "test", "test", [{rev, RevDoc1}]),
    RevDoc11 = proplists:get_value(<<"rev">>, Res),
    ?assertNot(RevDoc1 =:= RevDoc11),
    {ok, Attachment} = couchbeam:fetch_attachment(Db, "test", "test"),
    ?assertEqual( <<"test">>, Attachment),
    {ok, Doc2} = couchbeam:open_doc(Db, "test"),
    ?assertMatch({ok, {_}}, couchbeam:delete_attachment(Db, Doc2, "test")),
    Doc3 = {[{<<"_id">>, <<"test2">>}]},
    Doc4 = couchbeam_attachments:add_inline(Doc3, "test", "test.txt"),
    Doc5 = couchbeam_attachments:add_inline(Doc4, "test2", "test2.txt"),
    {ok, _} = couchbeam:save_doc(Db, Doc5),
    {ok, Attachment1} = couchbeam:fetch_attachment(Db, "test2", "test.txt"),
    {ok, Attachment2} = couchbeam:fetch_attachment(Db, "test2", "test2.txt"),
    ?assertEqual( <<"test">>, Attachment1),
    ?assertEqual( <<"test2">>, Attachment2),
    {ok, Doc6} = couchbeam:open_doc(Db, "test2"),
    Doc7 = couchbeam_attachments:delete_inline(Doc6, "test2.txt"),
    {ok, _} = couchbeam:save_doc(Db, Doc7),
    ?assertEqual({error, not_found}, couchbeam:fetch_attachment(Db, "test2", "test2.txt")),
    {ok, Attachment4} = couchbeam:fetch_attachment(Db, "test2", "test.txt"),
    ?assertEqual( <<"test">>, Attachment4),
    {ok, Doc8} = couchbeam:save_doc(Db, {[]}),

    TestFileName = data_path("1M"),
    {ok, FileInfo} = file:read_file_info(TestFileName),
    {ok, Fd} = file:open(TestFileName, [read]),
    FileSize = FileInfo#file_info.size,
    StreamFun = fun() ->
                    case file:read(Fd, 4096) of
                        {ok, Data} ->  {ok, iolist_to_binary(Data)};
                        _ -> eof
                    end
                end,
    {ok, _Res2} = couchbeam:put_attachment(Db, couchbeam_doc:get_id(Doc8), "1M", StreamFun,
                                          [{content_length, FileSize}, {rev, couchbeam_doc:get_rev(Doc8)}]),

    file:close(Fd),
    {ok, Doc9} = couchbeam:open_doc(Db, couchbeam_doc:get_id(Doc8)),
    Attachements = couchbeam_doc:get_value(<<"_attachments">>, Doc9),
    ?assert(Attachements /= undefined),
    Attachment5 = couchbeam_doc:get_value(<<"1M">>, Attachements),
    ?assert(Attachment5 /= undefined),
    ?assertEqual(FileSize, couchbeam_doc:get_value(<<"length">>, Attachment5)),
    {ok, Bin} = couchbeam:fetch_attachment(Db, couchbeam_doc:get_id(Doc8), "1M"),
    ?assertEqual(FileSize, iolist_size(Bin)),

    {ok, _Res3}= couchbeam:put_attachment(Db, "test/2", "test", "test"),
    {ok, Attachment10} = couchbeam:fetch_attachment(Db, "test/2", "test"),
    ?assertEqual(<<"test">>, Attachment10),
    {ok, _Res4}= couchbeam:put_attachment(Db, "test3", "test", "test"),
    {ok, Attachment11} = couchbeam:fetch_attachment(Db, "test3", "test"),
    ?assertEqual(<<"test">>, Attachment11),
    ok.

multipart_test() ->
    start_couchbeam_tests(),
    Server = couchbeam:server_connection(),

    {ok, Db} = couchbeam:create_db(Server, "couchbeam_testdb"),

    {ok, _Res4}= couchbeam:put_attachment(Db, "test", "test", "test"),
    Resp = couchbeam:open_doc(Db, <<"test">>, [{attachments, true}]),
    ?assertMatch( {ok, {multipart, _}}, Resp),

    {ok, {multipart, Stream}} = Resp,
    Collected = collect_mp(couchbeam:stream_doc(Stream), []),
    ?assert(proplists:is_defined(doc, Collected)),
    MpDoc = proplists:get_value(doc, Collected),
    MpDocId = couchbeam_doc:get_id(MpDoc),
    ?assertEqual(<<"test">>, MpDocId),
    ?assertEqual(<<"test">>, proplists:get_value(<<"test">>, Collected)),

    Resp1 = couchbeam:open_doc(Db, <<"test">>, [{open_revs, all},
                                                {accept, <<"multipart/mixed">>}]),
    ?assertMatch( {ok, {multipart, _}}, Resp1),
    {ok, {multipart, Stream1}} = Resp1,
    Collected1 = collect_mp(couchbeam:stream_doc(Stream1), []),
    MpDoc1 = proplists:get_value(doc, Collected1),
    MpDocId1 = couchbeam_doc:get_id(MpDoc1),
    ?assertEqual(<<"test">>, MpDocId1),
    ?assertEqual(<<"test">>, proplists:get_value(<<"test">>, Collected1)),
    {ok, Doc} = couchbeam:save_doc(Db, {[{<<"_id">>, <<"test2">>}]},
                                       [{<<"test.txt">>, <<"test">>}], []),
    ?assertEqual(<<"test2">>, couchbeam_doc:get_id(Doc)),
    {ok, MpAttachment1} = couchbeam:fetch_attachment(Db, <<"test2">>, <<"test.txt">>),
    ?assertEqual(<<"test">>, MpAttachment1),

    TestFileName = data_path("1M"),
    {ok, FileInfo} = file:read_file_info(TestFileName),
    FileSize = FileInfo#file_info.size,

    {ok, Doc1} = couchbeam:save_doc(Db, {[{<<"_id">>, <<"test5">>}]},
                                         [{<<"1M">>, {file, TestFileName}}], []),

    ?assert(couchbeam_doc:is_saved(Doc1)),
    {ok, Doc2} = couchbeam:open_doc(Db, <<"test5">>),
    MpAttachments = couchbeam_doc:get_value(<<"_attachments">>, Doc2),
    ?assert(MpAttachments /= undefined),
    MpAttachment2 = couchbeam_doc:get_value(<<"1M">>, MpAttachments),
    ?assert(MpAttachment2 /= undefined),
    ?assertEqual(FileSize, couchbeam_doc:get_value(<<"length">>, MpAttachment2)),
    {ok, MpBin} = couchbeam:fetch_attachment(Db, <<"test5">>, <<"1M">>),
    ?assertEqual(FileSize, iolist_size(MpBin)),
    {ok, MpDoc2} = couchbeam:save_doc(Db, Doc2, [{<<"hello.txt">>, <<"world">>}], []),
    ?assert(couchbeam_doc:is_saved(MpDoc2)),
    MpAttachments3= couchbeam_doc:get_value(<<"_attachments">>, MpDoc2),
    ?assert(MpAttachments3 /= undefined),
    MpAttachment4 = couchbeam_doc:get_value(<<"1M">>, MpAttachments3),
    ?assert(MpAttachment4 /= undefined),
    {ok, MpBin1} = couchbeam:fetch_attachment(Db, <<"test5">>, <<"1M">>),
    ?assertEqual(FileSize, iolist_size(MpBin1)),
    MpAttachment5 = couchbeam_doc:get_value(<<"hello.txt">>, MpAttachments3),
    ?assert(MpAttachment5 /= undefined),
    {ok, MpBin2} = couchbeam:fetch_attachment(Db, <<"test5">>, <<"hello.txt">>),
    ?assertEqual(<<"world">>, MpBin2),
    ok.

replicate_test() ->
    start_couchbeam_tests(),
    Server = couchbeam:server_connection(),

    {ok, Db} = couchbeam:create_db(Server, "couchbeam_testdb"),
    {ok, Db2} = couchbeam:create_db(Server, "couchbeam_testdb2"),

    {ok, Doc11} = couchbeam:save_doc(Db, {[]}),
    DocId11 = couchbeam_doc:get_id(Doc11),
    DocRev11 = couchbeam_doc:get_rev(Doc11),
    ?assertMatch({ok, _}, couchbeam:replicate(Server, Db, Db2)),

    {ok, Doc11_2} = couchbeam:open_doc(Db2, DocId11),
    DocRev11_2 = couchbeam_doc:get_rev(Doc11_2),
    ?assertEqual(DocRev11_2, DocRev11),

    {ok, Doc12} = couchbeam:save_doc(Db, Doc11 ),
    {ok, Doc13} = couchbeam:save_doc(Db, Doc12),

    DocRev12 = couchbeam_doc:get_rev(Doc12),
    DocRev13 = couchbeam_doc:get_rev(Doc13),
    {ok, Missing} = couchbeam:get_missing_revs(Db2, [{DocId11, [DocRev12,
                                                                DocRev13]}]),
    ?assertEqual([{DocId11, [DocRev12, DocRev13], [DocRev11]}], Missing),

    {ok, InstanceStartTime} = couchbeam:ensure_full_commit(Db),
    ?assert(is_binary(InstanceStartTime)),
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
collect_mp({att_eof, _Name, Next}, Acc) ->
    collect_mp(couchbeam:stream_doc(Next), Acc);
collect_mp(eof, Acc) ->
    Acc.

data_path(Basename) ->
    FName = filename:join([filename:dirname(filename:absname(?FILE)), "..", "support",
                           Basename]),

    case filelib:is_file(FName) of
        true -> FName;
        false ->
            F = filename:join([filename:dirname(code:which(?MODULE)), "..", "support",
                               Basename]),
            F
    end.


-endif.
