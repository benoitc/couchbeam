

# Module couchbeam_changes #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cancel_stream-1">cancel_stream/1</a></td><td></td></tr><tr><td valign="top"><a href="#follow-1">follow/1</a></td><td></td></tr><tr><td valign="top"><a href="#follow-2">follow/2</a></td><td>Stream changes to a pid.</td></tr><tr><td valign="top"><a href="#follow_once-1">follow_once/1</a></td><td></td></tr><tr><td valign="top"><a href="#follow_once-2">follow_once/2</a></td><td>fetch all changes at once using a normal or longpoll
connections.</td></tr><tr><td valign="top"><a href="#stream_next-1">stream_next/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cancel_stream-1"></a>

### cancel_stream/1 ###

`cancel_stream(Ref) -> any()`

<a name="follow-1"></a>

### follow/1 ###

<pre><code>
follow(Db::<a href="#type-db">db()</a>) -&gt; {ok, StreamRef::atom()} | {error, term()}
</code></pre>
<br />

<a name="follow-2"></a>

### follow/2 ###

<pre><code>
follow(Db::<a href="#type-db">db()</a>, Options::<a href="#type-changes_options">changes_options()</a>) -&gt; {ok, StreamRef::atom()} | {error, term()}
</code></pre>
<br />

Stream changes to a pid

Db : a db record

Client : pid  or callback where to send changes events where events are
The pid receive these events:



<dt>{change, StartRef, {done, Lastseq::integer()}</dt>




<dd>Connection terminated or you got all changes</dd>




<dt>{change, StartRef, Row :: ejson_object()}</dt>




<dd>Line of change</dd>




<dt>{error, LastSeq::integer(), Msg::term()}</dt>




<dd>Got an error, connection is closed when an error
happend.</dd>



LastSeq is the last sequence of changes.

While the callbac could be like:

```

       fun({done, LastSeq}) ->
           ok;
       fun({done, LastSeq}) ->
           ok;
       fun({done, LastSeq}) ->
           ok.
```


```
>Options :: changes_stream_options() [continuous
     | longpoll
     | normal
     | include_docs
     | {since, integer() | now}
     | {timeout, integer()}
     | heartbeat | {heartbeat, integer()}
     | {filter, string()} | {filter, string(), list({string(), string() | integer()})}
     | {view, string()},
     | {docids, list))},
     | {stream_to, pid()},
     | {async, once | normal}]
```

* `continuous | longpoll | normal`: set the type of changes
feed to get

* `include_doc`: if you want to include the doc in the line of
change

* `{timeout, Timeout::integer()}`: timeout

* `heartbeat | {heartbeat, Heartbeat::integer()}`: set couchdb
to send a heartbeat to maintain connection open

* `{filter, FilterName} | {filter, FilterName, Args::list({key,
value})}`: set the filter to use with optional arguments

* `{view, ViewName}`: use a view function as filter. Note
that it requires to set filter special value `"_view"`
to enable this feature.

* >`{stream_to, Pid}`: the pid where the changes will be sent,
by default the current pid. Used for continuous and longpoll
connections


Return {ok, StartRef, ChangesPid} or {error, Error}. Ref can be
used to disctint all changes from this pid. ChangesPid is the pid of
the changes loop process. Can be used to monitor it or kill it
when needed.

<a name="follow_once-1"></a>

### follow_once/1 ###

<pre><code>
follow_once(Db::<a href="#type-db">db()</a>) -&gt; {ok, LastSeq::integer(), Changes::list()} | {error, term()}
</code></pre>
<br />

<a name="follow_once-2"></a>

### follow_once/2 ###

<pre><code>
follow_once(Db::<a href="#type-db">db()</a>, Options::<a href="#type-changes_options">changes_options()</a>) -&gt; {ok, LastSeq::integer(), Changes::list()} | {error, term()}
</code></pre>
<br />

fetch all changes at once using a normal or longpoll
connections.

Db : a db record

```
Options :: changes_options() [
     | longpoll
     | normal
     | include_docs
     | {since, integer() | now}
     | {timeout, integer()}
     | heartbeat | {heartbeat, integer()}
     | {filter, string()}
     | {filter, string(), list({string(), string() | integer()})}
     | {docids, list()))},
     | {stream_to, pid()}
     ]
```

* `longpoll | normal`: set the type of changes
feed to get

* `include_docs`: if you want to include the doc in the line of
change

* `{timeout, Timeout::integer()}`: timeout

* `heartbeat | {heartbeat, Heartbeat::integer()}`: set couchdb
to send a heartbeat to maintain connection open

* `{filter, FilterName} | {filter, FilterName, Args::list({key,
value})`: set the filter to use with optional arguments

* `{view, ViewName}`: use a view function as filter. Note
that it requires to set filter special value `"_view"`
to enable this feature.


Result: `{ok, LastSeq::integer(), Rows::list()}` or
`{error, LastSeq, Error}`. LastSeq is the last sequence of changes.

<a name="stream_next-1"></a>

### stream_next/1 ###

`stream_next(Ref) -> any()`

