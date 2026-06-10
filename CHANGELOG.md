# Changelog

All notable changes to this project will be documented in this file.

## [2.1.0] - 2026-06-09

### Added

- `couchbeam_replicator`: a `gen_statem` pull-replication endpoint that imports
  documents (with full revision history and attachment bodies) from a source
  CouchDB into a pluggable local target behaviour, with resumable checkpoints.
  `couchbeam_replicator_ets` is an ets/dets reference target. Pull only.
  See `guides/replication.md`.

### Changed

- Updated to hackney 4.2.2. Response bodies are now read eagerly: a normal
  request returns `{ok, Status, Headers, Body}` with the body as a binary.
- Reworked pull-based response streaming (multipart `open_doc`,
  `fetch_attachment`/`stream_attachment` with `stream`, `stream_doc`) onto
  hackney's async mode. The public streaming API is unchanged.
- Replaced deprecated standalone `catch` expressions with `try ... catch`
  for OTP 29.

### Compatibility

- Requires OTP 27+. Dropped the OTP-version crypto shim and now always use
  `crypto:mac/4`.
- CI runs on OTP 27, 28 and 29.

### Testing

- End-to-end tests run the full client against a real CouchDB. `make e2e`
  starts CouchDB in Docker, runs the suite, and tears it down. CI runs all
  suite groups and they now gate the build.
- The e2e matrix runs the latest 3.x (`3`) and the `latest` CouchDB image, so
  CI adopts CouchDB 4.x automatically once it is released.

### Dependencies

- hackney: 4.2.2 (from 2.0.1)
- meck (test): 1.2.0 (from 0.9.2)
- Removed the optional `oauth` dependency.

### Removed

- OAuth support. The `{oauth, ...}` connection option and
  `couchbeam_util:oauth_header/3` are gone; CouchDB removed server-side OAuth
  in 2.x. Basic auth, proxy auth, and cookie auth are unchanged.
- `hackney:skip_body/1` usage (removed in hackney 4.x); bodies are read directly.

## [2.0.0] - 2026-01-21

### Breaking Changes

- **Requires OTP 27+**: Now uses the built-in `json` module instead of jsx/jiffy
- **Requires hackney 2.0.1+**: Uses hackney's new process-per-connection model
- **Connection handles are now PIDs**: `is_reference(Ref)` changed to `is_pid(Ref)` in connection handling
- **Document format**: All documents are now native Erlang maps with binary keys
- **API changes**:
  - `lookup_doc_rev/2` now returns `{ok, Rev}` tuple instead of bare revision
  - Removed deprecated functions and legacy code paths

### New Features

- **Comprehensive integration tests**: 29 new integration tests covering:
  - View streaming (`couchbeam_view:stream/3`, `foreach/4`, `show/4`)
  - Changes feed streaming (continuous, longpoll, heartbeat)
  - Replication operations
  - Database management (compact, ensure_full_commit, get_missing_revs)
  - Mango queries (find with selectors, fields, sort, pagination)
  - UUID generation (random, utc_random, server batch)
- **Streaming attachment support**: `stream_attachment/3` for efficient large file handling
- **Improved changes feed**: Simplified streaming using hackney's process-per-connection model
- **Better view streaming**: Low memory overhead with `json_stream_parse`

### Improvements

- Simplified architecture leveraging hackney's process-per-connection model
- Removed supervisor trees in favor of direct process management
- Updated CI to test on OTP 27.3, 28.0 across Linux, macOS, and FreeBSD
- Added GitHub Actions integration tests with real CouchDB instance
- Modernized documentation with usage examples

### Dependencies

- hackney: 2.0.1 (from process-per-connection branch)
- Removed jsx dependency (using OTP 27's built-in json module)

## [1.4.2] - Previous Release

See git history for changes prior to 2.0.0.
