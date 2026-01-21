# Couchbeam

A simple, idiomatic Erlang client for [Apache CouchDB](http://couchdb.apache.org) and [Barrel](https://barrel-db.org).

[![Hex.pm](https://img.shields.io/hexpm/v/couchbeam.svg)](https://hex.pm/packages/couchbeam)

## Quick Start

```erlang
%% Connect to CouchDB
Server = couchbeam:server_connection("http://localhost:5984"),
{ok, _} = couchbeam:server_info(Server),

%% Open a database
{ok, Db} = couchbeam:open_or_create_db(Server, "mydb"),

%% Save a document (documents are maps)
Doc = #{<<"_id">> => <<"hello">>, <<"message">> => <<"world">>},
{ok, Doc1} = couchbeam:save_doc(Db, Doc),

%% Fetch it back
{ok, Doc2} = couchbeam:open_doc(Db, "hello").
```

## Requirements

- **OTP 27+** (uses the built-in `json` module)
- **hackney 2.0.1+**

## Installation

Add to your `rebar.config`:

```erlang
{deps, [
    {couchbeam, "2.0.0"}
]}.
```

Then run `rebar3 compile`.

## Features

- Full CouchDB and Barrel API support
- Streaming views and changes feeds with low memory overhead
- Streaming attachment upload/download
- Documents represented as native Erlang maps
- Simple architecture using hackney's process-per-connection model

## Documentation

- [API Reference](https://hexdocs.pm/couchbeam/)
- [Migration Guide](https://hexdocs.pm/couchbeam/migration.html) - Upgrading from 1.x
- [Changes Feed Guide](https://hexdocs.pm/couchbeam/changes.html)
- [Views Guide](https://hexdocs.pm/couchbeam/views.html)

Generate docs locally: `rebar3 ex_doc`

## Usage Guide

### Starting the Application

In a release, add `couchbeam` to your application dependencies. For interactive use:

```erlang
1> application:ensure_all_started(couchbeam).
{ok, [crypto, asn1, public_key, ssl, hackney, couchbeam]}
```

### Connecting to CouchDB

```erlang
%% Simple connection
Server = couchbeam:server_connection("http://localhost:5984"),

%% With authentication
Server = couchbeam:server_connection("http://localhost:5984", [
    {basic_auth, {"admin", "password"}}
]).
```

### Working with Databases

```erlang
%% Create a database
{ok, Db} = couchbeam:create_db(Server, "mydb"),

%% Open existing database
{ok, Db} = couchbeam:open_db(Server, "mydb"),

%% Create if doesn't exist
{ok, Db} = couchbeam:open_or_create_db(Server, "mydb"),

%% Delete a database
ok = couchbeam:delete_db(Server, "mydb").
```

### Documents

Documents are Erlang maps with binary keys:

```erlang
%% Create a document
Doc = #{
    <<"_id">> => <<"mydoc">>,
    <<"type">> => <<"post">>,
    <<"title">> => <<"Hello World">>
},
{ok, Doc1} = couchbeam:save_doc(Db, Doc),

%% Update it
Doc2 = Doc1#{<<"title">> => <<"Updated Title">>},
{ok, Doc3} = couchbeam:save_doc(Db, Doc2),

%% Fetch a document
{ok, Doc4} = couchbeam:open_doc(Db, "mydoc"),

%% Delete a document
{ok, _} = couchbeam:delete_doc(Db, Doc4).
```

Use `couchbeam_doc` helpers for document manipulation:

```erlang
Id = couchbeam_doc:get_id(Doc),
Rev = couchbeam_doc:get_rev(Doc),
Value = couchbeam_doc:get_value(<<"title">>, Doc),
Doc2 = couchbeam_doc:set_value(<<"title">>, <<"New">>, Doc).
```

### Views

Fetch all results at once:

```erlang
%% All documents
{ok, Rows} = couchbeam_view:all(Db, [include_docs]),

%% Query a view
{ok, Rows} = couchbeam_view:fetch(Db, {<<"design">>, <<"viewname">>}, [
    {limit, 10},
    {startkey, <<"a">>},
    {endkey, <<"z">>}
]).
```

Stream results for large datasets:

```erlang
{ok, Ref} = couchbeam_view:stream(Db, {<<"design">>, <<"view">>}, []),

%% Receive rows as messages
receive
    {Ref, {row, Row}} -> handle_row(Row);
    {Ref, done} -> done;
    {Ref, {error, Reason}} -> handle_error(Reason)
end.
```

### Changes Feed

Get changes once:

```erlang
{ok, LastSeq, Changes} = couchbeam_changes:follow_once(Db, [include_docs]).
```

Stream continuous changes:

```erlang
{ok, Ref} = couchbeam_changes:follow(Db, [continuous, heartbeat]),

%% Receive changes as messages
receive
    {Ref, {change, Change}} -> handle_change(Change);
    {Ref, {done, LastSeq}} -> done;
    {Ref, {error, Reason}} -> handle_error(Reason)
end.
```

### Attachments

```erlang
%% Upload an attachment
{ok, _} = couchbeam:put_attachment(Db, "docid", "file.txt", <<"content">>, []),

%% Download an attachment
{ok, Data} = couchbeam:fetch_attachment(Db, "docid", "file.txt"),

%% Delete an attachment
{ok, Doc} = couchbeam:open_doc(Db, "docid"),
ok = couchbeam:delete_attachment(Db, Doc, "file.txt").
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `couchbeam` | Main API - connections, databases, documents |
| `couchbeam_doc` | Document manipulation helpers |
| `couchbeam_view` | View queries and streaming |
| `couchbeam_changes` | Changes feed |
| `couchbeam_attachments` | Inline attachment helpers |

## Contributing

Found a bug or have a feature request? [Open an issue](https://github.com/benoitc/couchbeam/issues).

## License

Apache License 2.0 - see [LICENSE](LICENSE) for details.

Copyright 2009-2026 Benoit Chesneau.
