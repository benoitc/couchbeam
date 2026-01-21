# Changes Feed Guide

This guide explains how to use the CouchDB changes feed with couchbeam.

## Overview

CouchDB provides a changes feed that notifies you of all document changes in a database.
The `couchbeam_changes` module provides both synchronous and asynchronous ways to
consume these changes.

## Quick Start

### Get All Changes (Synchronous)

```erlang
%% Get all changes up to now
{ok, LastSeq, Changes} = couchbeam_changes:follow_once(Db).

%% Process changes
lists:foreach(fun(Change) ->
    Id = maps:get(<<"id">>, Change),
    Seq = maps:get(<<"seq">>, Change),
    io:format("Doc ~s changed at seq ~p~n", [Id, Seq])
end, Changes).
```

### Follow Changes (Asynchronous)

```erlang
%% Start following changes
{ok, Ref} = couchbeam_changes:follow(Db, [continuous, heartbeat]),

%% Receive changes in a loop
loop(Ref) ->
    receive
        {Ref, {change, Change}} ->
            Id = maps:get(<<"id">>, Change),
            io:format("Change: ~s~n", [Id]),
            loop(Ref);
        {Ref, {done, LastSeq}} ->
            io:format("Done, last seq: ~p~n", [LastSeq]);
        {Ref, {error, Reason}} ->
            io:format("Error: ~p~n", [Reason])
    end.
```

## API Reference

### follow/1, follow/2, follow/3

Start following changes asynchronously.

- `Db` - Database record from `couchbeam:open_db/2,3`
- `Options` - List of options (see Options section)
- `To` - Process to receive messages (defaults to `self()`)

Returns `{ok, Ref}` where `Ref` is used to identify messages from this stream.

### follow_once/1, follow_once/2

Fetch all changes synchronously in a single request.

Returns `{ok, LastSeq, Changes}` or `{error, Reason}`.

### cancel/1

Cancel an active changes stream.

## Options

| Option | Description |
|--------|-------------|
| `continuous` | Keep connection open for real-time changes |
| `longpoll` | Long polling mode |
| `heartbeat` | Send empty line periodically to keep connection alive |
| `include_docs` | Include full document in each change |
| `{since, Seq}` | Start from this sequence number |
| `{since, now}` | Start from current sequence (new changes only) |
| `{filter, Name}` | Use a filter function |
| `{doc_ids, [Id]}` | Only return changes for these document IDs |
| `descending` | Return changes in descending order |
| `{limit, N}` | Maximum number of changes to return |

## Message Protocol

When using `follow/1,2,3`, messages are sent to the receiving process:

- `{Ref, {change, Change}}` - A change event (map with id, seq, changes, doc)
- `{Ref, {done, LastSeq}}` - Stream completed
- `{Ref, {error, Reason}}` - An error occurred

## Examples

### Simple Continuous Feed

```erlang
start_following(Db) ->
    {ok, Ref} = couchbeam_changes:follow(Db, [continuous, heartbeat]),
    loop(Ref).

loop(Ref) ->
    receive
        {Ref, {change, Change}} ->
            Id = maps:get(<<"id">>, Change),
            io:format("Changed: ~s~n", [Id]),
            loop(Ref);
        {Ref, {done, _}} ->
            ok
    end.
```

### Graceful Shutdown

```erlang
ok = couchbeam_changes:cancel(Ref).
```

## Process Lifecycle

The changes stream spawns a linked process that automatically terminates
if the owner dies, cleaning up the HTTP connection.
