# Views Guide

This guide explains how to query CouchDB views with couchbeam.

## Overview

CouchDB views are defined in design documents and allow you to query and index your
data. The `couchbeam_view` module provides both synchronous and streaming ways to
query views.

## Quick Start

### Fetch All Documents

```erlang
%% Get all documents in the database
{ok, Rows} = couchbeam_view:all(Db).

%% With options
{ok, Rows} = couchbeam_view:all(Db, [include_docs, {limit, 100}]).
```

### Query a View

```erlang
%% Query a named view
{ok, Rows} = couchbeam_view:fetch(Db, {<<"mydesign">>, <<"myview">>}).
```

### Stream Results

```erlang
%% Stream view results
{ok, Ref} = couchbeam_view:stream(Db, 'all_docs'),

loop(Ref) ->
    receive
        {Ref, {row, Row}} ->
            io:format("Row: ~p~n", [Row]),
            loop(Ref);
        {Ref, done} ->
            ok
    end.
```

## API Reference

### all/1, all/2

Fetch all documents from the database.

### fetch/1, fetch/2, fetch/3

Synchronously fetch all view results.

### stream/2, stream/3

Stream view results asynchronously.

### cancel_stream/1

Cancel an active view stream.

### count/1, count/2, count/3

Count documents in a view.

### first/1, first/2, first/3

Get only the first result from a view.

### fold/4, fold/5

Fold over view results with an accumulator.

### foreach/3, foreach/4

Apply a function to each row.

## Options

| Option | Description |
|--------|-------------|
| `{key, Key}` | Return only rows with this exact key |
| `{keys, [Key]}` | Return rows for multiple specific keys |
| `{start_key, Key}` | Start results from this key |
| `{end_key, Key}` | End results at this key |
| `{limit, N}` | Maximum number of rows to return |
| `{skip, N}` | Skip N rows before returning results |
| `descending` | Return results in descending order |
| `include_docs` | Include full documents in results |
| `reduce` | Use the reduce function |
| `group` | Group reduce results |

## Message Protocol

When using `stream/2,3`, messages are sent to the receiving process:

- `{Ref, {row, Row}}` - A row from the view (map with id, key, value, doc)
- `{Ref, done}` - All rows have been streamed
- `{Ref, {error, Reason}}` - An error occurred

## Examples

### Basic Queries

```erlang
%% All documents
{ok, Rows} = couchbeam_view:all(Db).

%% First 10 documents
{ok, Rows} = couchbeam_view:all(Db, [{limit, 10}]).
```

### Streaming Large Results

```erlang
{ok, Ref} = couchbeam_view:stream(Db, 'all_docs', [include_docs]),

process_stream(Ref) ->
    receive
        {Ref, {row, Row}} ->
            Doc = maps:get(<<"doc">>, Row),
            process_document(Doc),
            process_stream(Ref);
        {Ref, done} ->
            ok
    end.
```

## Process Lifecycle

View streams spawn a linked process that automatically terminates
if the owner dies, cleaning up the HTTP connection.
