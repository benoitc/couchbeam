

# Couchbeam - simple Barrel and Apache CouchDB client library for Erlang applications #

Copyright (c) 2009-2016 Benoît Chesneau.

__Version:__ 1.4.2

# couchbeam

Couchbeam is a simple erlang library for [Barrel](https://barrel-db.org) or [Apache CouchDB](http://couchdb.apache.org). Couchbeam provides you a full featured and easy client to access and manage multiple nodes.

#### Main features:

- Complete support of the BarrelDB and Apache CouchDB API
- Stream view results to your app
- Stream changes feeds
- reduced memory usage
- fetch and send attachments in a streaming fashion
- by default use the JSX module to encode/decode JSON
- support [Jiffy](http://github.com/davisp/jiffy) a JSON encoder/decoder
in C.

#### Useful modules are:

- [`couchbeam`](http://github.com/benoitc/couchbeam/blob/master/doc/couchbeam.md): The `couchbeam` module is the main interface for interaction with this application. It includes functions for managing connections to Apache CouchDB or RCOUCH servers and databases and for performing document creations, updates, deletes, views...
- [`couchbeam_doc`](http://github.com/benoitc/couchbeam/blob/master/doc/couchbeam_doc.md) Module to manipulate Documents structures. You can set values,
updates keys, ...
- [`couchbeam_attachments`](http://github.com/benoitc/couchbeam/blob/master/doc/couchbeam_attachments.md): Module to manipulate attachments. You can add, remove
attachments in a Document structure (inline attachments).
- [`couchbeam_view`](http://github.com/benoitc/couchbeam/blob/master/doc/couchbeam_view.md): Module to manage view results.
- [`couchbeam_changes`](http://github.com/benoitc/couchbeam/blob/master/doc/couchbeam_changes.md): Module to manage changes feeds. Follow continuously
the changes in a db or get all changes at once.

The goal of Couchbeam is to ease the access to the Apache CouchDB and RCOUCH HTTP API in erlang.

Read the [NEWS](https://raw.github.com/benoitc/couchbeam/master/NEWS) file
to get last changelog.

## Installation

Download the sources from our [Github repository](http://github.com/benoitc/couchbeam)

To build the application simply run 'make'. This should build .beam, .app
files and documentation.

To run tests run 'make test'.
To generate doc, run 'make doc'.

Or add it to your rebar config

```
   erlang
{deps, [
    ....
    {couchbeam, ".*", {git, "git://github.com/benoitc/couchbeam.git", {branch, "master"}}}
]}.
```

Note to compile with jiffy you need to define in the erlang options the
variable `WITH_JIFFY`.

if you use rebar, add to your `rebar.config`:

```
   erlang
{erl_opts, [{d, 'WITH_JIFFY'}]}.
```

or use the `rebar` command with the `-D` options:

```
   sh
rebar compile -DWITH_JIFFY
```

## Basic Usage

### Start couchbeam

Couchbeam is an [OTP](http://www.erlang.org/doc/design_principles/users_guide.html)
application. You have to start it first before using any of the
functions. The couchbeam application will start the default socket pool
for you.

To start in the console run:

```
   sh
$ erl -pa ebin
1> couchbeam:start().
ok
```

It will start hackney and all of the application it depends on:

```
   erlang
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

```
   erlang
Url = "http://localhost:5984",
Options = [],
S = couchbeam:server_connection(Url, Options).
```

Test the connection with `couchbeam:server_info/1` :

```
   erlang
{ok, _Version} = couchbeam:server_info(S).
```

### Open or Create a database

All document operations are done in databases. To open a database simply do:

```
   erlang
Options = [],
{ok, Db} = couchbeam:open_db(Server, "testdb", Options).
```

To create a new one:

```
   erlang
Options = [],
{ok, Db} = couchbeam:create_db(Server, "testdb", Options).
```

You can also use the shorcut `couchbeam:open_or_create_db/3`. that
will create a database if it does not exist.

### Make a new document

Make a new document:

```
   erlang
Doc = {[
{<<"_id">>, <<"test">>},
{<<"content">>, <<"some text">>}
]}.
```

And save it to the database:

```
   erlang
{ok, Doc1} = couchbeam:save_doc(Db, Doc).
```

The `couchbeam:save_doc/2` return a new document with updated
revision and if you do not specify the _id, a unique document id.

To change an document property use functions from `couchbeam_doc`.

### Retrieve a document

To retrieve a document do:

```
   erlang
{ok, Doc2} = couchbeam:open_doc(Db, "test").
```

If you want a specific revision:

```
   erlang
Rev = couchbeam_doc:get_rev(Doc1),
Options = [{rev, Rev}],
{ok, Doc3} = couchbeam:open_doc(Db, "test", Options).
```

Here we get the revision from the document we previously stored. Any
options from the Apache CouchDB and RCOUCH API can be used.

### Get all documents

To get all documents you have first to create an object
that will keep all informations.

```
   erlang
Options = [include_docs],
{ok, AllDocs} = couchbeam_view:all(Db, Options).
```

Ex of results:

```
   erlang
{ok,[{[{<<"id">>,<<"7a0ce91d0d0c5e5b51e904d1ee3266a3">>},
          {<<"key">>,<<"7a0ce91d0d0c5e5b51e904d1ee3266a3">>},
          {<<"value">>,
           {[{<<"rev">>,<<"15-15c0b3c4efa74f9a80d28ac040f18bdb">>}]}},
          {<<"doc">>,
           {[{<<"_id">>,<<"7a0ce91d0d0c5e5b51e904d1ee3266a3">>},
             {<<"_rev">>,<<"15-15c0b3c4efa74f9a80d28ac040f18"...>>}]}}]},
        ]}.
```

All functions to manipulate these results are in the `couchbeam_view` module.

### Couch DB views

Views are workin like all_docs. You have to create a View object before
doing anything.

```
   erlang
Options = [],
DesignName = "designname",
ViewName = "viewname",
{ok, ViewResults} = couchbeam_view:fetch(Db, {DesignName, ViewName}, Options).
```

Like the `all_docs` function, use the functions
from `couchbeam_view` module to manipulate results. You can pass
any querying options from the [view API](http://docs.rcouch.org/en/latest/api/ddoc/views.html).

Design doc are created like any documents:

```
   erlang
DesignDoc = {[
        {<<"_id">>, <<"_design/couchbeam">>},
        {<<"language">>,<<"javascript">>},
        {<<"views">>,
            {[{<<"test">>,
                {[{<<"map">>,
                    <<"function (doc) {\n if (doc.type == \"test\") {\n emit(doc._id, doc);\n}\n}">>
                }]}
            },{<<"test2">>,
                {[{<<"map">>,
                    <<"function (doc) {\n if (doc.type == \"test2\") {\n emit(doc._id, null);\n}\n}">>
                }]}
            }]}
        }
    ]},
{ok, DesignDoc1} = couchbeam:save_doc(Db, DesignDoc).
```

You can also use [couchapp](http://github.com/couchapp/couchapp) to manage them
more easily.

### Stream View results

While you can get results using `couchbeam_views:fetch/2`, you can also retrieve
all rows in a streaming fashion:

```
   erlang
ViewFun = fun(Ref, F) ->
    receive
        {Ref, done} ->
            io:format("done", []),
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

```
   erlang
DesignNam = "designname",
ViewName = "viewname",
{ok, StreamRef3} = couchbeam_view:stream(Db, {DesignNam, ViewName}, [include_docs]),
ViewFun(StreamRef3, ViewFun).
```

### Put, Fetch and Delete documents attachments

You can add attachments to any documents. Attachments could be anything.

To send an attachment:

```
   erlang
DocID = "test",
AttName = "test.txt",
Att = "some content I want to attach",
Options = []
{ok, _Result} = couchbeam:put_attachment(Db, DocId, AttName, Att, Options).
```

All attachments are streamed to servers. `Att` could be also be an iolist
or functions, see `couchbeam:put_attachment/5` for more information.

To fetch an attachment:

```
   erlang
{ok Att1} = couchbeam:fetch_attachment(Db, DocId, AttName).
```

You can use `couchbeam:stream_fetch_attachment/6` for the stream
fetch.

To delete an attachment:

```
   erlang
{ok, Doc4} = couchbeam:open_doc(Db, DocID),
ok = couchbeam:delete_attachment(Db, Doc4, AttName).
```

### Changes

Apache CouchDB and RCOUCH provide a means to get a list of changes made to documents in
the database. With couchbeam you can get changes using `couchbeam_changes:follow_once/2`.
This function returns all changes immediately. But you can also retrieve
all changes rows using longpolling :

```
   erlang
Options = [],
{ok, LastSeq, Rows} = couchbeam_changes:follow_once(Db, Options).
```

Options can be any Changes query parameters. See the [change API](http://docs.rcouch.org/en/latest/api/database/changes.html) for more informations.

You can also get [continuous](http://docs.rcouch.org/en/latest/api/database/changes.html#continuous):

```
   erlang
ChangesFun = fun(StreamRef, F) ->
    receive
        {StreamRef, {done, LastSeq}} ->
            io:format("stopped, last seq is ~p~n", [LastSeq]),
            ok;
        {StreamRef, {change, Change}} ->
            io:format("change row ~p ~n", [Change]),
            F(StreamRef, F);
        {StreamRef, Error}->
            io:format("error ? ~p ~n,", [Error])
    end
end,
Options = [continuous, heartbeat],
{ok, StreamRef} = couchbeam_changes:follow(Db, Options),
ChangesFun(StreamRef, ChangesFun).
```

> **Note**: a `gen_changes` behaviour exists in couchbeam that you can
use to create your own specific gen_server receiving changes. Have a
look in the
[example](https://github.com/benoitc/couchbeam/blob/master/examples/test_gen_changes.erl)
for more info.

### Authentication/ Connections options

You can authenticate to the database or Apache CouchDB or RCOUCH server by filling
options to the Option list in `couchbeam:server_connection/4` for the
server or in `couchbeam:create_db/3`, `couchbeam:open_db/3`,
`couchbeam:wopen_or_create_db/3` functions.

To set basic_auth on a server:

```
   erlang
UserName = "guest",
Password = "test",
Url = "http://localhost:5984",
Options = [{basic_auth, {UserName, Password}}],
S1 = couchbeam:server_connection(Url, Options).
```

Couchbeam support SSL, OAuth, Basic Authentication, and Proxy. You can
also set a cookie. For more informations about the options have a look
in the `couchbeam:server_connection/2` documentation.

## Contribute

For issues, comments or feedback please [create an
issue](http://github.com/benoitc/couchbeam/issues).
