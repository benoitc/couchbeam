# Pull replication endpoint

`couchbeam_replicator` is a `gen_statem` that pulls documents from a source
CouchDB into a local store of your choice, preserving each document's full
revision history. It is *pull only* (CouchDB to the app) and can be stopped and
resumed.

The local store is a module implementing the `couchbeam_replicator` behaviour.
`couchbeam_replicator_ets` is a reference target backed by an ets or dets table;
the same behaviour is how you would import into another store (for example
barrel_docdb).

## Quick start

```erlang
{ok, Server} = {ok, couchbeam:server_connection("http://127.0.0.1:5984", [])},
{ok, Src}    = couchbeam:open_db(Server, <<"mydb">>),

%% a persistent (dets) target so the import and checkpoint survive a restart
{ok, Pid} = couchbeam_replicator:start_link(
              Src, couchbeam_replicator_ets,
              #{table => my_import, backend => dets, file => "my_import.dets"},
              [{repl_id, <<"mydb->my_import">>}, {notify, self()}]),

receive {Pid, done} -> ok end.
```

Running the identical `start_link/4` again (after a restart, or to pick up new
changes) resumes from the stored checkpoint.

## API

```
couchbeam_replicator:start_link(Source, TargetMod, TargetArgs, Opts) -> {ok, Pid}
couchbeam_replicator:status(Pid)  -> {ok, #{state, repl_id, since, docs_written}}
couchbeam_replicator:pause(Pid)   -> ok
couchbeam_replicator:resume(Pid)  -> ok
couchbeam_replicator:stop(Pid)    -> ok
```

Options (`Opts`):

| Option | Default | Meaning |
| --- | --- | --- |
| `{mode, oneshot \| continuous}` | `oneshot` | stop when caught up, or keep polling |
| `{batch_size, N}` | `100` | changes per batch |
| `{interval, Ms}` | `5000` | continuous poll interval |
| `{repl_id, Bin}` | hash of source URL + target module | stable id used for resume |
| `{notify, Pid}` | none | receive progress messages |
| `{changes_options, L}` | `[]` | extra `_changes` query options (e.g. a filter) |

Progress messages sent to `{notify, Pid}` are tagged with the replicator pid:
`{Pid, {checkpoint, Seq}}`, `{Pid, {docs_written, N}}`, `{Pid, done}`,
`{Pid, {error, Reason}}`.

## How resume works

Resume needs no `_local` document on the source; the target owns the checkpoint.

1. Each replication has a stable `repl_id` (pass `{repl_id, Bin}`, or it defaults
   to a hash of the source URL and the target module).
2. After each batch is written, the engine calls
   `write_checkpoint(ReplId, LastSeq, State)`; `Seq` is CouchDB's opaque
   `last_seq`.
3. On start, `read_checkpoint(ReplId, State)` returns the last seq (or `nil`), and
   the changes feed continues from there.
4. Every batch goes through `revs_diff` before fetching, so replaying a change is
   a no-op: a stale checkpoint costs a few redundant checks, never duplicates.

Use the `dets` backend (or your own persistent store) for resume across restarts;
`ets` resumes within the node's lifetime.

## Writing a target

A target module implements:

```erlang
-behaviour(couchbeam_replicator).

init(Args)                            -> {ok, State} | {error, term()}.
revs_diff(IdRevs, State)              -> {ok, Missing, State}.
   %% IdRevs :: [{DocId, [Rev]}]; return the subset not yet stored.
write_docs(Entries, State)            -> {ok, State} | {error, term()}.
   %% Entries :: [{Doc, Atts}]; Doc is a map with _id/_rev/_revisions/(_deleted)/
   %% _attachments; Atts :: #{AttName => Bin} holds the attachment bodies as
   %% delivered (the doc's _attachments metadata records content type/encoding).
read_checkpoint(ReplId, State)        -> {ok, Seq | nil, State}.
write_checkpoint(ReplId, Seq, State)  -> {ok, State}.
terminate(Reason, State)              -> any().   %% optional
```

Each stored document keeps its `_revisions` field, so the full revision history
is available to reconstruct the document's revision tree.

## ets/dets reference target

`couchbeam_replicator_ets` stores, in a `set` table:

```
{{doc, DocId, Rev}, {Doc, Atts}}
{{leafs, DocId}, [Rev]}
{{checkpoint, ReplId}, Seq}
```

`init/1` args: `#{table => Name, backend => ets | dets, file => Path}`
(`backend` defaults to `ets`). Inspect a table with
`couchbeam_replicator_ets:get_doc/3`, `leaf_revs/2`, and `checkpoint/2`.

An ets table created by the target is owned by the replicator process and is
deleted when a one-shot run finishes. To keep the result (or to resume later),
create the named table yourself first (it is reused) or use the `dets` backend.
