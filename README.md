

# Couchbeam - simple Apache CouchDB client library for Erlang applications #

Copyright (c) 2009-2026 Benoit Chesneau.

__Version:__ 2.0.0

# couchbeam

Couchbeam is a simple erlang library for [Barrel](https://barrel-db.org) or [Apache CouchDB](http://couchdb.apache.org). Couchbeam provides you a full featured and easy client to access and manage multiple nodes.

## Requirements

- **OTP 27 or later** - Uses the stdlib `json` module for JSON encoding/decoding
- **hackney 2.0.1** - HTTP client with process-per-connection model

## Main features

- Complete support of the BarrelDB and Apache CouchDB API
- Stream view results to your app
- Stream changes feeds
- Reduced memory usage with streaming
- Fetch and send attachments in a streaming fashion
- JSON represented as Erlang maps (OTP 27+ json module)
- Simple architecture using hackney's process-per-connection model

## Useful modules

- `couchbeam`: The main interface for interaction with this application. Includes functions for managing connections to Apache CouchDB or RCOUCH servers and databases and for performing document creations, updates, deletes, views...
- `couchbeam_doc`: Module to manipulate document structures (maps). Set values, update keys, etc.
- `couchbeam_attachments`: Module to manipulate attachments. Add, remove attachments in a document structure (inline attachments).
- `couchbeam_view`: Module to manage view results.
- `couchbeam_changes`: Module to manage changes feeds. Follow continuously the changes in a db or get all changes at once.

## Documentation

Full API documentation is available on [HexDocs](https://hexdocs.pm/couchbeam/).

**Guides:**
- Migration Guide - Migrate from 1.x to 2.0
- Changes Feed Guide - How to use the changes feed
- Views Guide - How to query views

Generate documentation locally with:

```sh
rebar3 ex_doc
```

## Installation

Download the sources from our [Github repository](http://github.com/benoitc/couchbeam)

To build the application simply run 'make'. This should build .beam, .app
files and documentation.

To run tests run 'make test'.
To generate doc, run 'make doc'.

Or add it to your rebar config

```erlang
{deps, [
    {couchbeam, "2.0.0"}
]}.
```

## Basic Usage

### Start couchbeam

Couchbeam is an [OTP](http://www.erlang.org/doc/design_principles/users_guide.html)
application. You have to start it first before using any of the
functions. The couchbeam application will start the default socket pool
for you.

To start in the console run:

```sh
$ erl -pa ebin
1> couchbeam:start().
ok
```

It will start hackney and all of the application it depends on:

```erlang
application:start(crypto),
application:start(asn1),
application:start(public_key),
application:start(ssl),
application:start(hackney),
application:start(couchbeam).
```

Or add couchbeam to the applications property of your .app in a release

### Create a connection to the server

To create a connection to a server machine:

```erlang
Url = "http://localhost:5984",
Options = [],
S = couchbeam:server_connection(Url, Options).
```

Test the connection with `couchbeam:server_info/1`:

```erlang
{ok, _Version} = couchbeam:server_info(S).
```

### Open or Create a database

All document operations are done in databases. To open a database simply do:

```erlang
Options = [],
{ok, Db} = couchbeam:open_db(Server, "testdb", Options).
```

To create a new one:

```erlang
Options = [],
{ok, Db} = couchbeam:create_db(Server, "testdb", Options).
```

You can also use the shortcut `couchbeam:open_or_create_db/3` that
will create a database if it does not exist.

### Make a new document

Make a new document (documents are maps in couchbeam 2.0):

```erlang
Doc = #{
    <<"_id">> => <<"test">>,
    <<"content">> => <<"some text">>
}.
```

And save it to the database:

```erlang
{ok, Doc1} = couchbeam:save_doc(Db, Doc).
```

The `couchbeam:save_doc/2` return a new document with updated
revision and if you do not specify the _id, a unique document id.

To change a document property, use map syntax:

```erlang
Doc2 = Doc1#{<<"content">> => <<"updated text">>}.
```

### Retrieve a document

To retrieve a document do:

```erlang
{ok, Doc2} = couchbeam:open_doc(Db, "test").
```

If you want a specific revision:

```erlang
Rev = maps:get(<<"_rev">>, Doc1),
Options = [{rev, Rev}],
{ok, Doc3} = couchbeam:open_doc(Db, "test", Options).
```

Here we get the revision from the document we previously stored. Any
options from the Apache CouchDB and RCOUCH API can be used.

### Get all documents

To get all documents:

```erlang
Options = [include_docs],
{ok, AllDocs} = couchbeam_view:all(Db, Options).
```

Ex of results:

```erlang
{ok, [
    #{
        <<"id">> => <<"7a0ce91d0d0c5e5b51e904d1ee3266a3">>,
        <<"key">> => <<"7a0ce91d0d0c5e5b51e904d1ee3266a3">>,
        <<"value">> => #{<<"rev">> => <<"15-15c0b3c4efa74f9a80d28ac040f18bdb">>},
        <<"doc">> => #{
            <<"_id">> => <<"7a0ce91d0d0c5e5b51e904d1ee3266a3">>,
            <<"_rev">> => <<"15-15c0b3c4efa74f9a80d28ac040f18...">>
        }
    },
    ...
]}.
```

All functions to manipulate these results are in the `couchbeam_view` module.

### CouchDB views

Views are working like all_docs. You have to specify the design name and view name:

```erlang
Options = [],
DesignName = "designname",
ViewName = "viewname",
{ok, ViewResults} = couchbeam_view:fetch(Db, {DesignName, ViewName}, Options).
```

Like the `all/2` function, use the functions from `couchbeam_view` module to manipulate results. You can pass any querying options from the [view API](http://docs.rcouch.org/en/latest/api/ddoc/views.html).

Design docs are created like any documents:

```erlang
DesignDoc = #{
    <<"_id">> => <<"_design/couchbeam">>,
    <<"language">> => <<"javascript">>,
    <<"views">> => #{
        <<"test">> => #{
            <<"map">> => <<"function (doc) { if (doc.type == \"test\") { emit(doc._id, doc); }}">>
        },
        <<"test2">> => #{
            <<"map">> => <<"function (doc) { if (doc.type == \"test2\") { emit(doc._id, null); }}">>
        }
    }
},
{ok, DesignDoc1} = couchbeam:save_doc(Db, DesignDoc).
```

You can also use [couchapp](http://github.com/couchapp/couchapp) to manage them
more easily.

### Stream View results

While you can get results using `couchbeam_view:fetch/2`, you can also retrieve
all rows in a streaming fashion:

```erlang
ViewFun = fun(Ref, F) ->
    receive
        {Ref, done} ->
            io:format("done~n"),
            done;
        {Ref, {row, Row}} ->
            io:format("got ~p~n", [Row]),
            F(Ref, F);
        {error, Ref, Error} ->
            io:format("error: ~p~n", [Error])
    end
end,

{ok, StreamRef} = couchbeam_view:stream(Db, 'all_docs'),
ViewFun(StreamRef, ViewFun),
{ok, StreamRef2} = couchbeam_view:stream(Db, 'all_docs', [include_docs]),
ViewFun(StreamRef2, ViewFun).
```

You can of course do the same with a view:

```erlang
DesignName = "designname",
ViewName = "viewname",
{ok, StreamRef3} = couchbeam_view:stream(Db, {DesignName, ViewName}, [include_docs]),
ViewFun(StreamRef3, ViewFun).
```

### Put, Fetch and Delete documents attachments

You can add attachments to any documents. Attachments could be anything.

To send an attachment:

```erlang
DocID = "test",
AttName = "test.txt",
Att = "some content I want to attach",
Options = [],
{ok, _Result} = couchbeam:put_attachment(Db, DocId, AttName, Att, Options).
```

All attachments are streamed to servers. `Att` could be also be an iolist
or functions, see `couchbeam:put_attachment/5` for more information.

To fetch an attachment:

```erlang
{ok, Att1} = couchbeam:fetch_attachment(Db, DocId, AttName).
```

You can use `couchbeam:stream_fetch_attachment/6` for the stream
fetch.

To delete an attachment:

```erlang
{ok, Doc4} = couchbeam:open_doc(Db, DocID),
ok = couchbeam:delete_attachment(Db, Doc4, AttName).
```

### Changes

Apache CouchDB and RCOUCH provide a means to get a list of changes made to documents in
the database. With couchbeam you can get changes using `couchbeam_changes:follow_once/2`.
This function returns all changes immediately. But you can also retrieve
all changes rows using longpolling:

```erlang
Options = [],
{ok, LastSeq, Rows} = couchbeam_changes:follow_once(Db, Options).
```

Options can be any Changes query parameters. See the [change API](http://docs.rcouch.org/en/latest/api/database/changes.html) for more information.

You can also get [continuous](http://docs.rcouch.org/en/latest/api/database/changes.html#continuous) changes:

```erlang
ChangesFun = fun(StreamRef, F) ->
    receive
        {StreamRef, {done, LastSeq}} ->
            io:format("stopped, last seq is ~p~n", [LastSeq]),
            ok;
        {StreamRef, {change, Change}} ->
            io:format("change row ~p~n", [Change]),
            F(StreamRef, F);
        {StreamRef, Error}->
            io:format("error ? ~p~n", [Error])
    end
end,
Options = [continuous, heartbeat],
{ok, StreamRef} = couchbeam_changes:follow(Db, Options),
ChangesFun(StreamRef, ChangesFun).
```

See the [Changes Feed Guide](doc/guides/changes.md) for more details.

### Authentication/Connections options

You can authenticate to the database or Apache CouchDB or RCOUCH server by filling
options to the Option list in `couchbeam:server_connection/4` for the
server or in `couchbeam:create_db/3`, `couchbeam:open_db/3`,
`couchbeam:open_or_create_db/3` functions.

To set basic_auth on a server:

```erlang
UserName = "guest",
Password = "test",
Url = "http://localhost:5984",
Options = [{basic_auth, {UserName, Password}}],
S1 = couchbeam:server_connection(Url, Options).
```

Couchbeam support SSL, OAuth, Basic Authentication, and Proxy. You can
also set a cookie. For more information about the options have a look
in the `couchbeam:server_connection/2` documentation.

## Contribute

For issues, comments or feedback please [create an
issue](http://github.com/benoitc/couchbeam/issues).

