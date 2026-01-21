# Changelog

All notable changes to this project will be documented in this file.

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
