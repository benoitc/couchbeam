# Migration Guide: Couchbeam 1.x to 2.0

This guide helps you migrate your application from couchbeam 1.x to 2.0.

## Requirements

### OTP Version

Couchbeam 2.0 requires **OTP 27 or later**. This is because couchbeam now uses the
`json` module from OTP's stdlib instead of external JSON libraries (jsx/jiffy).

Check your OTP version:

```erlang
erlang:system_info(otp_release).
%% Should return "27" or higher
```

### Dependencies

Update your `rebar.config`:

```erlang
{deps, [
    {couchbeam, "2.0.0"},
    {hackney, "2.0.1"}
]}.
```

The following dependencies are **no longer required**:
- `jsx` - removed, OTP json module is used
- `jiffy` - removed, OTP json module is used

## JSON Representation Changes

The most significant change in couchbeam 2.0 is that **all JSON is now represented
as Erlang maps** instead of proplists.

### Before (1.x - proplists)

```erlang
%% Creating a document
Doc = {[
    {<<"_id">>, <<"mydoc">>},
    {<<"name">>, <<"John">>},
    {<<"age">>, 30}
]}.

%% Accessing fields
Id = proplists:get_value(<<"_id">>, element(1, Doc)).
Name = couchbeam_doc:get_value(<<"name">>, Doc).

%% Saving
{ok, Doc1} = couchbeam:save_doc(Db, Doc).
Rev = couchbeam_doc:get_rev(Doc1).
```

### After (2.0 - maps)

```erlang
%% Creating a document
Doc = #{
    <<"_id">> => <<"mydoc">>,
    <<"name">> => <<"John">>,
    <<"age">> => 30
}.

%% Accessing fields
Id = maps:get(<<"_id">>, Doc).
Name = maps:get(<<"name">>, Doc).

%% Saving
{ok, Doc1} = couchbeam:save_doc(Db, Doc).
Rev = maps:get(<<"_rev">>, Doc1).
```

## View Results

View results are now maps:

### Before (1.x)

```erlang
{ok, Rows} = couchbeam_view:fetch(Db, {<<"design">>, <<"view">>}),
lists:foreach(fun(Row) ->
    Id = proplists:get_value(<<"id">>, element(1, Row))
end, Rows).
```

### After (2.0)

```erlang
{ok, Rows} = couchbeam_view:fetch(Db, {<<"design">>, <<"view">>}),
lists:foreach(fun(Row) ->
    Id = maps:get(<<"id">>, Row)
end, Rows).
```

## Changes Feed

Changes feed results are now maps:

### Before (1.x)

```erlang
{ok, Ref} = couchbeam_changes:follow(Db, [continuous]),
receive
    {Ref, {change, Change}} ->
        Seq = proplists:get_value(<<"seq">>, element(1, Change))
end.
```

### After (2.0)

```erlang
{ok, Ref} = couchbeam_changes:follow(Db, [continuous]),
receive
    {Ref, {change, Change}} ->
        Seq = maps:get(<<"seq">>, Change)
end.
```

## Removed Modules

The following modules have been removed:

| Removed Module | Replacement |
|----------------|-------------|
| `gen_changes` | Use `couchbeam_changes:follow/1,2,3` directly |
| `couchbeam_changes_stream` | Internal, replaced by linked process |
| `couchbeam_changes_sup` | No longer needed |
| `couchbeam_view_stream` | Internal, replaced by linked process |
| `couchbeam_view_sup` | No longer needed |

## Common Migration Patterns

### Pattern 1: Document Field Access

```erlang
%% Before
get_name(Doc) ->
    couchbeam_doc:get_value(<<"name">>, Doc, <<"Unknown">>).

%% After
get_name(Doc) ->
    maps:get(<<"name">>, Doc, <<"Unknown">>).
```

### Pattern 2: Building Documents

```erlang
%% Before
build_doc(Id, Name) ->
    {[{<<"_id">>, Id}, {<<"name">>, Name}]}.

%% After
build_doc(Id, Name) ->
    #{<<"_id">> => Id, <<"name">> => Name}.
```

### Pattern 3: Updating Documents

```erlang
%% Before
update_doc(Doc, NewName) ->
    couchbeam_doc:set_value(<<"name">>, NewName, Doc).

%% After
update_doc(Doc, NewName) ->
    Doc#{<<"name">> => NewName}.
```

## Testing Your Migration

After migrating, run the test suite:

```bash
rebar3 eunit
rebar3 dialyzer
```
