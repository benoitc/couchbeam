

# Module couchbeam_view #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-1">all/1</a></td><td>fetch all docs.</td></tr><tr><td valign="top"><a href="#all-2">all/2</a></td><td>fetch all docs.</td></tr><tr><td valign="top"><a href="#cancel_stream-1">cancel_stream/1</a></td><td></td></tr><tr><td valign="top"><a href="#count-1">count/1</a></td><td>Equivalent to <a href="#count-3"><tt>count(Db, all_docs, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#count-2">count/2</a></td><td>Equivalent to <a href="#count-3"><tt>count(Db, ViewName, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#count-3">count/3</a></td><td>count number of doc in a view (or all docs).</td></tr><tr><td valign="top"><a href="#fetch-1">fetch/1</a></td><td>Equivalent to <a href="#fetch-3"><tt>fetch(Db, all_docs, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#fetch-2">fetch/2</a></td><td>Equivalent to <a href="#fetch-3"><tt>fetch(Db, ViewName, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#fetch-3">fetch/3</a></td><td>Collect view results.</td></tr><tr><td valign="top"><a href="#first-1">first/1</a></td><td>Equivalent to <a href="#first-3"><tt>first(Db, all_docs, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#first-2">first/2</a></td><td>Equivalent to <a href="#first-3"><tt>first(Db, ViewName, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#first-3">first/3</a></td><td>get first result of a view.</td></tr><tr><td valign="top"><a href="#fold-4">fold/4</a></td><td>Equivalent to <a href="#fold-5"><tt>fold(Function, Acc, Db, ViewName, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#fold-5">fold/5</a></td><td>call Function(Row, AccIn) on succesive row, starting with
AccIn == Acc.</td></tr><tr><td valign="top"><a href="#foreach-3">foreach/3</a></td><td>Equivalent to <a href="#foreach-4"><tt>foreach(Function, Db, ViewName, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#foreach-4">foreach/4</a></td><td>call Function(Row) on succesive row.</td></tr><tr><td valign="top"><a href="#parse_view_options-1">parse_view_options/1</a></td><td>parse view options.</td></tr><tr><td valign="top"><a href="#stream-2">stream/2</a></td><td>Equivalent to <a href="#stream-4"><tt>stream(Db, ViewName, Client, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#stream-3">stream/3</a></td><td>stream view results to a pid.</td></tr><tr><td valign="top"><a href="#stream_next-1">stream_next/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all-1"></a>

### all/1 ###

<pre><code>
all(Db::<a href="#type-db">db()</a>) -&gt; {ok, Rows::[<a href="#type-ejson_object">ejson_object()</a>]} | {error, term()}
</code></pre>
<br />

Equivalent to [`fetch(Db, all_docs, [])`](#fetch-3).

fetch all docs

<a name="all-2"></a>

### all/2 ###

<pre><code>
all(Db::<a href="#type-db">db()</a>, Options::<a href="#type-view_options">view_options()</a>) -&gt; {ok, Rows::[<a href="#type-ejson_object">ejson_object()</a>]} | {error, term()}
</code></pre>
<br />

Equivalent to [`fetch(Db, all_docs, Options)`](#fetch-3).

fetch all docs

<a name="cancel_stream-1"></a>

### cancel_stream/1 ###

`cancel_stream(Ref) -> any()`

<a name="count-1"></a>

### count/1 ###

<pre><code>
count(Db::<a href="#type-db">db()</a>) -&gt; integer() | {error, term()}
</code></pre>
<br />

Equivalent to [`count(Db, all_docs, [])`](#count-3).

<a name="count-2"></a>

### count/2 ###

<pre><code>
count(Db::<a href="#type-db">db()</a>, ViewName::all_docs | {DesignName::string(), ViewName::string()}) -&gt; integer() | {error, term()}
</code></pre>
<br />

Equivalent to [`count(Db, ViewName, [])`](#count-3).

<a name="count-3"></a>

### count/3 ###

<pre><code>
count(Db::<a href="#type-db">db()</a>, ViewName::all_docs | {DesignName::string(), ViewName::string()}, Options::<a href="#type-view_options">view_options()</a>) -&gt; integer() | {error, term()}
</code></pre>
<br />

count number of doc in a view (or all docs)

<a name="fetch-1"></a>

### fetch/1 ###

<pre><code>
fetch(Db::<a href="#type-db">db()</a>) -&gt; {ok, Rows::[<a href="#type-ejson_object">ejson_object()</a>]} | {error, term()}
</code></pre>
<br />

Equivalent to [`fetch(Db, all_docs, [])`](#fetch-3).

<a name="fetch-2"></a>

### fetch/2 ###

<pre><code>
fetch(Db::<a href="#type-db">db()</a>, ViewName::all_docs | {DesignName::string(), ViewName::string()}) -&gt; {ok, Rows::[<a href="#type-ejson_object">ejson_object()</a>]} | {error, term()}
</code></pre>
<br />

Equivalent to [`fetch(Db, ViewName, [])`](#fetch-3).

<a name="fetch-3"></a>

### fetch/3 ###

<pre><code>
fetch(Db::<a href="#type-db">db()</a>, ViewName::all_docs | {DesignName::string(), ViewName::string()}, Options::<a href="#type-view_options">view_options()</a>) -&gt; {ok, Rows::[<a href="#type-ejson_object">ejson_object()</a>]} | {error, term()}
</code></pre>
<br />

Collect view results

Db: a db record

ViewName: `'all_docs'` to get all docs or `{DesignName,
ViewName}`


```
Options :: view_options() [{key, binary()} | {start_docid, binary()}
     | {end_docid, binary()} | {start_key, binary()}
     | {end_key, binary()} | {limit, integer()}
     | {stale, stale()}
     | descending
     | {skip, integer()}
     | group | {group_level, integer()}
     | {inclusive_end, boolean()} | {reduce, boolean()} | reduce | include_docs | conflicts
     | {keys, list(binary())}
```


See [`couchbeam_view:stream/4`](couchbeam_view.md#stream-4) for more information about
options.

Return: {ok, Rows} or {error, Rows, Error}

<a name="first-1"></a>

### first/1 ###

<pre><code>
first(Db::<a href="#type-db">db()</a>) -&gt; {ok, Row::<a href="#type-ejson_object">ejson_object()</a>} | {error, term()}
</code></pre>
<br />

Equivalent to [`first(Db, all_docs, [])`](#first-3).

<a name="first-2"></a>

### first/2 ###

<pre><code>
first(Db::<a href="#type-db">db()</a>, ViewName::all_docs | {DesignName::string(), ViewName::string()}) -&gt; {ok, Row::<a href="#type-ejson_object">ejson_object()</a>} | {error, term()}
</code></pre>
<br />

Equivalent to [`first(Db, ViewName, [])`](#first-3).

<a name="first-3"></a>

### first/3 ###

<pre><code>
first(Db::<a href="#type-db">db()</a>, ViewName::all_docs | {DesignName::string(), ViewName::string()}, Options::<a href="#type-view_options">view_options()</a>) -&gt; {ok, Rows::<a href="#type-ejson_object">ejson_object()</a>} | {error, term()}
</code></pre>
<br />

get first result of a view

Db: a db record

ViewName: 'all_docs' to get all docs or {DesignName,
ViewName}


```
Options :: view_options() [{key, binary()} | {start_docid, binary()}
     | {end_docid, binary()} | {start_key, binary()}
     | {end_key, binary()} | {limit, integer()}
     | {stale, stale()}
     | descending
     | {skip, integer()}
     | group | {group_level, integer()}
     | {inclusive_end, boolean()} | {reduce, boolean()} | reduce | include_docs | conflicts
     | {keys, list(binary())}
```


See [`couchbeam_view:stream/4`](couchbeam_view.md#stream-4) for more information about
options.

Return: {ok, Row} or {error, Error}

<a name="fold-4"></a>

### fold/4 ###

<pre><code>
fold(Function::function(), Acc::any(), Db::<a href="#type-db">db()</a>, ViewName::all_docs | {DesignName::string(), ViewName::string()}) -&gt; [term()] | {error, term()}
</code></pre>
<br />

Equivalent to [`fold(Function, Acc, Db, ViewName, [])`](#fold-5).

<a name="fold-5"></a>

### fold/5 ###

<pre><code>
fold(Function::function(), Acc::any(), Db::<a href="#type-db">db()</a>, ViewName::all_docs | {DesignName::string(), ViewName::string()}, Options::<a href="#type-view_options">view_options()</a>) -&gt; [term()] | {error, term()}
</code></pre>
<br />

call Function(Row, AccIn) on succesive row, starting with
AccIn == Acc. Function/2 must return a new list accumultator or the
atom _done_ to stop fetching results. Acc0 is returned if the
list is empty. For example:

```
  couchbeam_view:fold(fun(Row, Acc) -> [Row|Acc] end, [], Db, 'all_docs').
```

<a name="foreach-3"></a>

### foreach/3 ###

<pre><code>
foreach(Function::function(), Db::<a href="#type-db">db()</a>, ViewName::all_docs | {DesignName::string(), ViewName::string()}) -&gt; [term()] | {error, term()}
</code></pre>
<br />

Equivalent to [`foreach(Function, Db, ViewName, [])`](#foreach-4).

<a name="foreach-4"></a>

### foreach/4 ###

<pre><code>
foreach(Function::function(), Db::<a href="#type-db">db()</a>, ViewName::all_docs | {DesignName::string(), ViewName::string()}, Options::<a href="#type-view_options">view_options()</a>) -&gt; [term()] | {error, term()}
</code></pre>
<br />

call Function(Row) on succesive row. Example:

```
  couchbeam_view:foreach(fun(Row) -> io:format("got row ~p~n", [Row]) end, Db, 'all_docs').
```

<a name="parse_view_options-1"></a>

### parse_view_options/1 ###

<pre><code>
parse_view_options(Options::list()) -&gt; <a href="#type-view_query_args">view_query_args()</a>
</code></pre>
<br />

parse view options

<a name="stream-2"></a>

### stream/2 ###

<pre><code>
stream(Db::<a href="#type-db">db()</a>, ViewName::all_docs | {DesignName::string(), ViewName::string()}) -&gt; {ok, StartRef::term(), ViewPid::pid()} | {error, term()}
</code></pre>
<br />

Equivalent to [`stream(Db, ViewName, Client, [])`](#stream-4).

<a name="stream-3"></a>

### stream/3 ###

<pre><code>
stream(Db::<a href="#type-db">db()</a>, ViewName::all_docs | {DesignName::string(), ViewName::string()}, Options::<a href="#type-view_options">view_options()</a>) -&gt; {ok, StartRef::term(), ViewPid::pid()} | {error, term()}
</code></pre>
<br />

stream view results to a pid

Db: a db record

ViewName: 'all_docs' to get all docs or {DesignName,
ViewName}

Client: pid where to send view events where events are:



<dt>{row, StartRef, done}</dt>




<dd>All view results have been fetched</dd>




<dt>{row, StartRef, Row :: ejson_object()}</dt>




<dd>A row in the view</dd>




<dt>{error, StartRef, Error}</dt>




<dd>Got an error, connection is closed when an error
happend.</dd>



```
Options :: view_options() [{key, binary()} | {start_docid, binary()}
     | {end_docid, binary()} | {start_key, binary()}
     | {end_key, binary()} | {limit, integer()}
     | {stale, stale()}
     | descending
     | {skip, integer()}
     | group | {group_level, integer()}
     | {inclusive_end, boolean()} | {reduce, boolean()} | reduce | include_docs | conflicts
     | {keys, list(binary())}
     | <code>{stream_to, Pid}</code>: the pid where the changes will be sent,
       by default the current pid. Used for continuous and longpoll
       connections
```

* `{key, Key}`: key value

* `{start_docid, DocId}`: document id to start with (to allow pagination
for duplicate start keys

* `{end_docid, DocId}`: last document id to include in the result (to
allow pagination for duplicate endkeys)

* `{start_key, Key}`: start result from key value

* `{end_key, Key}`: end result from key value

* `{limit, Limit}`: Limit the number of documents in the result

* `{stale, Stale}`: If stale=ok is set, CouchDB will not refresh the view
even if it is stale, the benefit is a an improved query latency. If
stale=update_after is set, CouchDB will update the view after the stale
result is returned. If stale=false is set, CouchDB will update the view before
the query. The default value of this parameter is update_after.

* `descending`: reverse the result

* `{skip, N}`: skip n number of documents

* `group`: the reduce function reduces to a single result
row.

* `{group_level, Level}`: the reduce function reduces to a set
of distinct keys.

* `{reduce, boolean()}`: whether to use the reduce function of the view. It defaults to
true, if a reduce function is defined and to false otherwise.

* `include_docs`: automatically fetch and include the document
which emitted each view entry

* `{inclusive_end, boolean()}`: Controls whether the endkey is included in
the result. It defaults to true.

* `conflicts`: include conflicts

* `{keys, [Keys]}`: to pass multiple keys to the view query


Return `{ok, StartRef, ViewPid}` or `{error,
Error}`. Ref can be
used to disctint all changes from this pid. ViewPid is the pid of
the view loop process. Can be used to monitor it or kill it
when needed.

<a name="stream_next-1"></a>

### stream_next/1 ###

`stream_next(Ref) -> any()`

