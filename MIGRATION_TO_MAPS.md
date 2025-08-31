# Migration Guide: From Proplists to Maps

## Important Breaking Changes

Couchbeam has been updated to use the native Erlang maps instead of the proplists for the JSON representation. This is a **breaking change** that will require to update your code.

## Why the Maps?

- **Better Performance**: The O(log n) key lookup versus O(n) for the proplists
- **Less Memory**: The maps use less memory for the documents
- **More Clean Syntax**: The native Erlang syntax for the data access
- **Modern Erlang**: It aligns with the current Erlang/OTP best practices

## What Has Changed

### 1. The Document Structure

**Before (with Proplists):**
```erlang
Doc = {[{<<"_id">>, <<"mydoc">>}, {<<"title">>, <<"Hello">>}]}.
```

**After (with Maps):**
```erlang
Doc = #{<<"_id">> => <<"mydoc">>, <<"title">> => <<"Hello">>}.
```

### 2. The Document API

**For Creating the Documents:**
```erlang
% The old way (it does not work anymore)
Doc = {[{<<"_id">>, <<"mydoc">>}]}.

% The new way
Doc = couchbeam_doc:new(<<"mydoc">>).
% Or directly with the maps
Doc = #{<<"_id">> => <<"mydoc">>}.
```

**For Getting the Values:**
```erlang
% It still works (the API is compatible)
Title = couchbeam_doc:get_value(<<"title">>, Doc).

% Also you can use the native map functions
Title = maps:get(<<"title">>, Doc, undefined).
```

**For Setting the Values:**
```erlang
% It still works (the API is compatible)  
NewDoc = couchbeam_doc:set_value(<<"title">>, <<"World">>, Doc).

% Also you can use the native map syntax
NewDoc = Doc#{<<"title">> => <<"World">>}.
```

### 3. The View Results

**Before:**
```erlang
case couchbeam_view:fetch(Db, ViewName) of
    {ok, {[{<<"rows">>, Rows}|_]}} ->
        % To process the rows as proplists
        lists:foreach(fun({Row}) ->
            Id = proplists:get_value(<<"id">>, Row)
        end, Rows)
end.
```

**After:**
```erlang
case couchbeam_view:fetch(Db, ViewName) of
    {ok, Rows} when is_list(Rows) ->
        % The rows are now the maps
        lists:foreach(fun(#{<<"id">> := Id, <<"value">> := Value}) ->
            % To process the row
        end, Rows)
end.
```

### 4. The Changes Feed

**Before:**
```erlang
{ok, Ref} = couchbeam_changes:follow(Db),
receive
    {change, Ref, {[{<<"seq">>, Seq}, {<<"id">>, Id}|_]}} ->
        % To process the change
end.
```

**After:**
```erlang
{ok, Ref} = couchbeam_changes:follow(Db),
receive
    {change, Ref, #{<<"seq">> := Seq, <<"id">> := Id} = Change} ->
        % To process the change as map
end.
```

### 5. The Attachments

**Before:**
```erlang
Doc = {[{<<"_id">>, <<"mydoc">>}]},
NewDoc = couchbeam_attachments:add_inline(Doc, Data, <<"file.txt">>).
```

**After:**
```erlang
Doc = #{<<"_id">> => <<"mydoc">>},
NewDoc = couchbeam_attachments:add_inline(Doc, Data, <<"file.txt">>).
% The attachments are now stored like: #{<<"_attachments">> => #{<<"file.txt">> => AttData}}
```

## The Migration Steps

### Step 1: To Update the Document Creation

You must replace all the proplist document creation:
```erlang
% You find the patterns like:
{[{<<"_id">>, Id}]}
{[]}

% You replace with:
#{<<"_id">> => Id}
#{}
```

### Step 2: To Update the Pattern Matching

```erlang
% The old pattern matching
process_doc({[{<<"_id">>, Id}|_]}) ->
    ...

% The new pattern matching  
process_doc(#{<<"_id">> := Id} = Doc) ->
    ...
```

### Step 3: To Update the Value Access

```erlang
% The old proplist access
{Props} = Doc,
Value = proplists:get_value(<<"key">>, Props).

% The new map access
Value = maps:get(<<"key">>, Doc, undefined).
% Or you use the couchbeam_doc API (it still works)
Value = couchbeam_doc:get_value(<<"key">>, Doc).
```

### Step 4: To Update the JSON Encoding/Decoding

```erlang
% The couchbeam_ejson now returns the maps
Json = couchbeam_ejson:decode(Binary),  % It returns a map
Binary = couchbeam_ejson:encode(Map).    % It accepts a map
```

### Step 5: To Update the Tests

You update your test assertions:
```erlang
% The old
?assertEqual({[{<<"_id">>, <<"test">>}]}, Doc).

% The new
?assertEqual(#{<<"_id">> => <<"test">>}, Doc).
```

## The Streaming Is Still Supported

The streaming capabilities, they remain intact:

```erlang
% The views still support the streaming
{ok, Ref} = couchbeam_view:stream(Db, ViewName),
receive
    {Ref, {row, Row}} ->
        % The Row is now a map
        #{<<"id">> := Id, <<"value">> := Value} = Row
end.

% The changes feed still supports the streaming
{ok, Ref} = couchbeam_changes:follow(Db, [continuous]),
receive
    {change, Ref, Change} ->
        % The Change is now a map
        #{<<"seq">> := Seq} = Change
end.
```

## The Common Patterns

### To Build the Complex Documents

```erlang
% To create a document with the nested structures
Doc = #{
    <<"_id">> => <<"user:123">>,
    <<"type">> => <<"user">>,
    <<"profile">> => #{
        <<"name">> => <<"John Doe">>,
        <<"email">> => <<"john@example.com">>
    },
    <<"tags">> => [<<"erlang">>, <<"couchdb">>]
}.
```

### To Merge the Documents

```erlang
% To merge two documents
Doc1 = #{<<"a">> => 1},
Doc2 = #{<<"b">> => 2},
Merged = couchbeam_doc:merge(Doc1, Doc2).
% The result: #{<<"a">> => 1, <<"b">> => 2}
```

### To Work with the Attachments

```erlang
Doc = couchbeam_doc:new(<<"mydoc">>),
% To add the attachment
DocWithAtt = couchbeam_attachments:add_inline(Doc, <<"content">>, <<"file.txt">>),
% The attachments are in: #{<<"_attachments">> => #{<<"file.txt">> => ...}}
```

## The Troubleshooting

### Error: badarg

If you see the `badarg` errors, probably you are passing a proplist where a map is expected:
```erlang
% It is wrong
couchbeam_doc:get_value(<<"key">>, {[{<<"key">>, <<"value">>}]}).

% It is right  
couchbeam_doc:get_value(<<"key">>, #{<<"key">> => <<"value">>}).
```

### The Pattern Match Failures

You must update your pattern matches:
```erlang
% This will fail with the maps
case Doc of
    {Props} -> ...

% You use this instead
case Doc of
    #{} = Map -> ...
```

## The Performance Improvements

With the maps, you can expect:
- The faster document field access
- The reduced memory usage for the large documents
- The better performance when you work with the nested structures
- The more efficient JSON parsing and encoding

## You Need Help?

If you encounter the issues during the migration:
1. You check that all the document creation uses the maps
2. You verify the pattern matching has been updated
3. You ensure the JSON decoding expectations are updated
4. You review the test assertions for the map structures

For the complex migrations, you can consider:
- The gradual module-by-module updates
- To create the helper functions for the transition
- To run both the old and the new code in parallel during the migration