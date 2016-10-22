

# Module couchbeam #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-doc_stream">doc_stream()</a> ###


__abstract datatype__: `doc_stream()`




### <a name="type-mp_attachments">mp_attachments()</a> ###


<pre><code>
mp_attachments() = {Name::binary(), Bin::binary()} | {Name::binary(), Bin::binary(), Encoding::binary()} | {Name::binary(), Bin::binary(), Type::binary(), Encoding::binary()} | {Name::binary(), {file, Path::string()}} | {Name::binary(), {file, Path::string()}, Encoding::binary()} | {Name::binary(), Fun::function(), Length::integer()} | {Name::binary(), Fun::function(), Length::integer(), Encoding::binary()} | {Name::binary(), Fun::function(), Length::integer(), Type::binary(), Encoding::binary()} | {Name::binary(), {Fun::function(), Acc::any()}, Length::integer()} | {Name::binary(), {Fun::function(), Acc::any()}, Length::integer(), Encoding::binary()} | {Name::binary(), {Fun::function(), Acc::any()}, Length::integer(), Type::binary(), Encoding::binary()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_dbs-1">all_dbs/1</a></td><td>get list of databases on a CouchDB node.</td></tr><tr><td valign="top"><a href="#all_dbs-2">all_dbs/2</a></td><td>get list of databases on a CouchDB node with optional filter.</td></tr><tr><td valign="top"><a href="#compact-1">compact/1</a></td><td>Compaction compresses the database file by removing unused
sections created during updates.</td></tr><tr><td valign="top"><a href="#compact-2">compact/2</a></td><td>Like compact/1 but this compacts the view index from the
current version of the design document.</td></tr><tr><td valign="top"><a href="#copy_doc-2">copy_doc/2</a></td><td>duplicate a document using the doc API.</td></tr><tr><td valign="top"><a href="#copy_doc-3">copy_doc/3</a></td><td>copy a doc to a destination.</td></tr><tr><td valign="top"><a href="#create_db-2">create_db/2</a></td><td>Create a database and a client for connectiong to it.</td></tr><tr><td valign="top"><a href="#create_db-3">create_db/3</a></td><td>Create a database and a client for connectiong to it.</td></tr><tr><td valign="top"><a href="#create_db-4">create_db/4</a></td><td>Create a database and a client for connectiong to it.</td></tr><tr><td valign="top"><a href="#db_exists-2">db_exists/2</a></td><td>test if db with dbname exists on the CouchDB node.</td></tr><tr><td valign="top"><a href="#db_info-1">db_info/1</a></td><td>get database info.</td></tr><tr><td valign="top"><a href="#delete_attachment-3">delete_attachment/3</a></td><td>delete a document attachment.</td></tr><tr><td valign="top"><a href="#delete_attachment-4">delete_attachment/4</a></td><td>delete a document attachment.</td></tr><tr><td valign="top"><a href="#delete_db-1">delete_db/1</a></td><td>delete database.</td></tr><tr><td valign="top"><a href="#delete_db-2">delete_db/2</a></td><td>delete database.</td></tr><tr><td valign="top"><a href="#delete_doc-2">delete_doc/2</a></td><td>delete a document.</td></tr><tr><td valign="top"><a href="#delete_doc-3">delete_doc/3</a></td><td>delete a document
if you want to make sure the doc it emptied on delete, use the option
{empty_on_delete,  true} or pass a doc with just _id and _rev
members.</td></tr><tr><td valign="top"><a href="#delete_docs-2">delete_docs/2</a></td><td>delete a list of documents.</td></tr><tr><td valign="top"><a href="#delete_docs-3">delete_docs/3</a></td><td>delete a list of documents
if you want to make sure the doc it emptied on delete, use the option
{empty_on_delete,  true} or pass a doc with just _id and _rev
members.</td></tr><tr><td valign="top"><a href="#design_info-2">design_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#doc_exists-2">doc_exists/2</a></td><td>test if doc with uuid exists in the given db.</td></tr><tr><td valign="top"><a href="#end_doc_stream-1">end_doc_stream/1</a></td><td>stop to receive the multipart response of the doc api and close
the connection.</td></tr><tr><td valign="top"><a href="#ensure_full_commit-1">ensure_full_commit/1</a></td><td>commit all docs in memory.</td></tr><tr><td valign="top"><a href="#ensure_full_commit-2">ensure_full_commit/2</a></td><td>commit all docs in memory.</td></tr><tr><td valign="top"><a href="#fetch_attachment-3">fetch_attachment/3</a></td><td>fetch a document attachment.</td></tr><tr><td valign="top"><a href="#fetch_attachment-4">fetch_attachment/4</a></td><td>fetch a document attachment
Options are
<ul>
<li><code>stream</code>: to start streaming an attachment. the function return
<code>{ok, Ref}</code> where is a ref to the attachment</li>
<li>Other options that can be sent using the REST API</li>
</ul>.</td></tr><tr><td valign="top"><a href="#get_missing_revs-2">get_missing_revs/2</a></td><td>get missing revisions.</td></tr><tr><td valign="top"><a href="#get_uuid-1">get_uuid/1</a></td><td>Get one uuid from the server.</td></tr><tr><td valign="top"><a href="#get_uuids-2">get_uuids/2</a></td><td>Get a list of uuids from the server.</td></tr><tr><td valign="top"><a href="#lookup_doc_rev-2">lookup_doc_rev/2</a></td><td>get the last revision of the document.</td></tr><tr><td valign="top"><a href="#lookup_doc_rev-3">lookup_doc_rev/3</a></td><td></td></tr><tr><td valign="top"><a href="#open_db-2">open_db/2</a></td><td>Create a client for connection to a database.</td></tr><tr><td valign="top"><a href="#open_db-3">open_db/3</a></td><td>Create a client for connection to a database.</td></tr><tr><td valign="top"><a href="#open_doc-2">open_doc/2</a></td><td>open a document.</td></tr><tr><td valign="top"><a href="#open_doc-3">open_doc/3</a></td><td>open a document
Params is a list of query argument.</td></tr><tr><td valign="top"><a href="#open_or_create_db-2">open_or_create_db/2</a></td><td>Create a client for connecting to a database and create the
database if needed.</td></tr><tr><td valign="top"><a href="#open_or_create_db-3">open_or_create_db/3</a></td><td>Create a client for connecting to a database and create the
database if needed.</td></tr><tr><td valign="top"><a href="#open_or_create_db-4">open_or_create_db/4</a></td><td>Create a client for connecting to a database and create the
database if needed.</td></tr><tr><td valign="top"><a href="#put_attachment-4">put_attachment/4</a></td><td>put an attachment.</td></tr><tr><td valign="top"><a href="#put_attachment-5">put_attachment/5</a></td><td>put an attachment.</td></tr><tr><td valign="top"><a href="#replicate-2">replicate/2</a></td><td>Handle replication.</td></tr><tr><td valign="top"><a href="#replicate-3">replicate/3</a></td><td>Handle replication.</td></tr><tr><td valign="top"><a href="#replicate-4">replicate/4</a></td><td>handle Replication.</td></tr><tr><td valign="top"><a href="#save_doc-2">save_doc/2</a></td><td>save a document.</td></tr><tr><td valign="top"><a href="#save_doc-3">save_doc/3</a></td><td>save a *document
A document is a Json object like this one:.</td></tr><tr><td valign="top"><a href="#save_doc-4">save_doc/4</a></td><td>save a *document with all its attacjments
A document is a Json object like this one:.</td></tr><tr><td valign="top"><a href="#save_docs-2">save_docs/2</a></td><td>save a list of documents.</td></tr><tr><td valign="top"><a href="#save_docs-3">save_docs/3</a></td><td>save a list of documents.</td></tr><tr><td valign="top"><a href="#send_attachment-2">send_attachment/2</a></td><td>send an attachment chunk
Msg could be Data, eof to stop sending.</td></tr><tr><td valign="top"><a href="#server_connection-0">server_connection/0</a></td><td>Create a server for connectiong to a CouchDB node.</td></tr><tr><td valign="top"><a href="#server_connection-1">server_connection/1</a></td><td></td></tr><tr><td valign="top"><a href="#server_connection-2">server_connection/2</a></td><td>Create a server for connectiong to a CouchDB node.</td></tr><tr><td valign="top"><a href="#server_connection-4">server_connection/4</a></td><td>Create a server for connectiong to a CouchDB node.</td></tr><tr><td valign="top"><a href="#server_info-1">server_info/1</a></td><td>Get Information from the server.</td></tr><tr><td valign="top"><a href="#stream_attachment-1">stream_attachment/1</a></td><td>fetch an attachment chunk.</td></tr><tr><td valign="top"><a href="#stream_doc-1">stream_doc/1</a></td><td>stream the multipart response of the doc API.</td></tr><tr><td valign="top"><a href="#view_cleanup-1">view_cleanup/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_dbs-1"></a>

### all_dbs/1 ###

<pre><code>
all_dbs(Server::<a href="#type-server">server()</a>) -&gt; {ok, iolist()}
</code></pre>
<br />

get list of databases on a CouchDB node

<a name="all_dbs-2"></a>

### all_dbs/2 ###

<pre><code>
all_dbs(Server::<a href="#type-server">server()</a>, Options::<a href="#type-view_options">view_options()</a>) -&gt; {ok, iolist()}
</code></pre>
<br />

get list of databases on a CouchDB node with optional filter

<a name="compact-1"></a>

### compact/1 ###

<pre><code>
compact(Db::<a href="#type-db">db()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Compaction compresses the database file by removing unused
sections created during updates.
See [`http://wiki.apache.org/couchdb/Compaction`](http://wiki.apache.org/couchdb/Compaction) for more informations

<a name="compact-2"></a>

### compact/2 ###

<pre><code>
compact(Db::<a href="#type-db">db()</a>, ViewName::string()) -&gt; ok | {error, term()}
</code></pre>
<br />

Like compact/1 but this compacts the view index from the
current version of the design document.
See [`http://wiki.apache.org/couchdb/Compaction#View_compaction`](http://wiki.apache.org/couchdb/Compaction#View_compaction) for more informations

<a name="copy_doc-2"></a>

### copy_doc/2 ###

`copy_doc(Db, Doc) -> any()`

duplicate a document using the doc API

<a name="copy_doc-3"></a>

### copy_doc/3 ###

`copy_doc(Db, Doc, Dest) -> any()`

copy a doc to a destination. If the destination exist it will
use the last revision, in other case a new doc is created with the
the current doc revision.

<a name="create_db-2"></a>

### create_db/2 ###

`create_db(Server, DbName) -> any()`

Equivalent to [`create_db(Server, DbName, [], [])`](#create_db-4).

Create a database and a client for connectiong to it.

<a name="create_db-3"></a>

### create_db/3 ###

`create_db(Server, DbName, Options) -> any()`

Equivalent to [`create_db(Server, DbName, Options, [])`](#create_db-4).

Create a database and a client for connectiong to it.

<a name="create_db-4"></a>

### create_db/4 ###

<pre><code>
create_db(Server::<a href="#type-server">server()</a>, DbName::string(), Options::<a href="#type-optionList">optionList()</a>, Params::list()) -&gt; {ok, <a href="#type-db">db()</a> | {error, Error}}
</code></pre>
<br />

Create a database and a client for connectiong to it.

Connections are made to:

```
          http://Host:PortPrefix/DbName
```

If ssl is set https is used. See server_connections for options.
Params is a list of optionnal query argument you want to pass to the
db. Useful for bigcouch for example.

<a name="db_exists-2"></a>

### db_exists/2 ###

<pre><code>
db_exists(Server::<a href="#type-server">server()</a>, DbName::string()) -&gt; boolean()
</code></pre>
<br />

test if db with dbname exists on the CouchDB node

<a name="db_info-1"></a>

### db_info/1 ###

<pre><code>
db_info(Db::<a href="#type-db">db()</a>) -&gt; {ok, iolist() | {error, Error}}
</code></pre>
<br />

get database info

<a name="delete_attachment-3"></a>

### delete_attachment/3 ###

`delete_attachment(Db, Doc, Name) -> any()`

Equivalent to [`delete_attachment(Db, Doc, Name, [])`](#delete_attachment-4).

delete a document attachment

<a name="delete_attachment-4"></a>

### delete_attachment/4 ###

`delete_attachment(Db, DocOrDocId, Name, Options) -> any()`

delete a document attachment

<a name="delete_db-1"></a>

### delete_db/1 ###

`delete_db(Db) -> any()`

Equivalent to [`delete_db(Server, DbName)`](#delete_db-2).

delete database

<a name="delete_db-2"></a>

### delete_db/2 ###

<pre><code>
delete_db(Server::<a href="#type-server">server()</a>, DbName) -&gt; {ok, iolist() | {error, Error}}
</code></pre>
<br />

delete database

<a name="delete_doc-2"></a>

### delete_doc/2 ###

`delete_doc(Db, Doc) -> any()`

Equivalent to [`delete_doc(Db, Doc, [])`](#delete_doc-3).

delete a document

<a name="delete_doc-3"></a>

### delete_doc/3 ###

<pre><code>
delete_doc(Db, Doc, Options) -&gt; {ok, Result} | {error, Error}
</code></pre>
<br />

delete a document
if you want to make sure the doc it emptied on delete, use the option
{empty_on_delete,  true} or pass a doc with just _id and _rev
members.

<a name="delete_docs-2"></a>

### delete_docs/2 ###

`delete_docs(Db, Docs) -> any()`

Equivalent to [`delete_docs(Db, Docs, [])`](#delete_docs-3).

delete a list of documents

<a name="delete_docs-3"></a>

### delete_docs/3 ###

<pre><code>
delete_docs(Db::<a href="#type-db">db()</a>, Docs::list(), Options::list()) -&gt; {ok, Result} | {error, Error}
</code></pre>
<br />

delete a list of documents
if you want to make sure the doc it emptied on delete, use the option
{empty_on_delete,  true} or pass a doc with just _id and _rev
members.

<a name="design_info-2"></a>

### design_info/2 ###

`design_info(Db, DesignName) -> any()`

<a name="doc_exists-2"></a>

### doc_exists/2 ###

<pre><code>
doc_exists(Db::<a href="#type-db">db()</a>, DocId::string()) -&gt; boolean()
</code></pre>
<br />

test if doc with uuid exists in the given db

<a name="end_doc_stream-1"></a>

### end_doc_stream/1 ###

<pre><code>
end_doc_stream(X1::<a href="#type-doc_stream">doc_stream()</a>) -&gt; ok
</code></pre>
<br />

stop to receive the multipart response of the doc api and close
the connection.

<a name="ensure_full_commit-1"></a>

### ensure_full_commit/1 ###

`ensure_full_commit(Db) -> any()`

Equivalent to [`ensure_full_commit(Db, [])`](#ensure_full_commit-2).

commit all docs in memory

<a name="ensure_full_commit-2"></a>

### ensure_full_commit/2 ###

<pre><code>
ensure_full_commit(Db::<a href="#type-db">db()</a>, Options::list()) -&gt; {ok, InstancestartTime::binary()} | {error, term()}
</code></pre>
<br />

commit all docs in memory

<a name="fetch_attachment-3"></a>

### fetch_attachment/3 ###

`fetch_attachment(Db, DocId, Name) -> any()`

Equivalent to [`fetch_attachment(Db, DocId, Name, [])`](#fetch_attachment-4).

fetch a document attachment

<a name="fetch_attachment-4"></a>

### fetch_attachment/4 ###

<pre><code>
fetch_attachment(Db::<a href="#type-db">db()</a>, DocId::string(), Name::string(), Options0::list()) -&gt; {ok, binary()} | {ok, atom()} | {error, term()}
</code></pre>
<br />

fetch a document attachment
Options are

* `stream`: to start streaming an attachment. the function return
`{ok, Ref}` where is a ref to the attachment

* Other options that can be sent using the REST API



<a name="get_missing_revs-2"></a>

### get_missing_revs/2 ###

<pre><code>
get_missing_revs(Db::#db{}, IdRevs::[{binary(), [binary()]}]) -&gt; {ok, [{DocId::binary(), [MissingRev::binary()], [PossibleAncestor::binary()]}]} | {error, term()}
</code></pre>
<br />

get missing revisions

<a name="get_uuid-1"></a>

### get_uuid/1 ###

<pre><code>
get_uuid(Server::<a href="#type-server">server()</a>) -&gt; <a href="#type-lists">lists()</a>
</code></pre>
<br />

Get one uuid from the server

<a name="get_uuids-2"></a>

### get_uuids/2 ###

<pre><code>
get_uuids(Server::<a href="#type-server">server()</a>, Count::integer()) -&gt; <a href="#type-lists">lists()</a>
</code></pre>
<br />

Get a list of uuids from the server

<a name="lookup_doc_rev-2"></a>

### lookup_doc_rev/2 ###

`lookup_doc_rev(Db, DocId) -> any()`

get the last revision of the document

<a name="lookup_doc_rev-3"></a>

### lookup_doc_rev/3 ###

`lookup_doc_rev(Db, DocId, Params) -> any()`

<a name="open_db-2"></a>

### open_db/2 ###

`open_db(Server, DbName) -> any()`

Equivalent to [`open_db(Server, DbName, [])`](#open_db-3).

Create a client for connection to a database

<a name="open_db-3"></a>

### open_db/3 ###

<pre><code>
open_db(Server::<a href="#type-server">server()</a>, DbName::string(), Options::<a href="#type-optionList">optionList()</a>) -&gt; {ok, <a href="#type-db">db()</a>}
</code></pre>
<br />

Create a client for connection to a database

<a name="open_doc-2"></a>

### open_doc/2 ###

`open_doc(Db, DocId) -> any()`

Equivalent to [`open_doc(Db, DocId, [])`](#open_doc-3).

open a document

<a name="open_doc-3"></a>

### open_doc/3 ###

<pre><code>
open_doc(Db::<a href="#type-db">db()</a>, DocId::string(), Params::list()) -&gt; {ok, Doc} | {error, Error}
</code></pre>
<br />

open a document
Params is a list of query argument. Have a look in CouchDb API

<a name="open_or_create_db-2"></a>

### open_or_create_db/2 ###

`open_or_create_db(Server, DbName) -> any()`

Equivalent to [`open_or_create_db(Server, DbName, [], [])`](#open_or_create_db-4).

Create a client for connecting to a database and create the
database if needed.

<a name="open_or_create_db-3"></a>

### open_or_create_db/3 ###

`open_or_create_db(Server, DbName, Options) -> any()`

Equivalent to [`open_or_create_db(Server, DbName, Options, [])`](#open_or_create_db-4).

Create a client for connecting to a database and create the
database if needed.

<a name="open_or_create_db-4"></a>

### open_or_create_db/4 ###

<pre><code>
open_or_create_db(Server::<a href="#type-server">server()</a>, DbName0::string(), Options::list(), Params::list()) -&gt; {ok, <a href="#type-db">db()</a> | {error, Error}}
</code></pre>
<br />

Create a client for connecting to a database and create the
database if needed.

<a name="put_attachment-4"></a>

### put_attachment/4 ###

`put_attachment(Db, DocId, Name, Body) -> any()`

Equivalent to [`put_attachment(Db, DocId, Name, Body, [])`](#put_attachment-5).

put an attachment

<a name="put_attachment-5"></a>

### put_attachment/5 ###

<pre><code>
put_attachment(Db::<a href="#type-db">db()</a>, DocId::string(), Name::string(), Body::<a href="#type-body">body()</a>, Option::<a href="#type-optionList">optionList()</a>) -&gt; {ok, iolist()}
</code></pre>

<ul class="definitions"><li><code><a name="type-optionList">optionList()</a> = [<a href="#type-option">option()</a>]</code></li><li><code><a name="type-option">option()</a> = {rev, string()} | {content_type, string()} | {content_length, string()}</code></li><li><code><a name="type-body">body()</a> = [] | string() | binary() | <a href="#type-fun_arity_0">fun_arity_0()</a> | {<a href="#type-fun_arity_1">fun_arity_1()</a>, <a href="#type-initial_state">initial_state()</a>, stream}</code></li><li><code><a name="type-initial_state">initial_state()</a> = term()</code></li></ul>

put an attachment

<a name="replicate-2"></a>

### replicate/2 ###

<pre><code>
replicate(Server::<a href="#type-server">server()</a>, RepObj::{list()}) -&gt; {ok, Result} | {error, Error}
</code></pre>
<br />

Handle replication. Pass an object containting all informations
It allows to pass for example an authentication info

```
  RepObj = {[
  {<<"source">>, <<"sourcedb">>},
  {<<"target">>, <<"targetdb">>},
  {<<"create_target">>, true}
  ]}
  replicate(Server, RepObj).
```


<a name="replicate-3"></a>

### replicate/3 ###

<pre><code>
replicate(Server::<a href="#type-server">server()</a>, Source::string(), Target::<a href="#type-target">target()</a>) -&gt; {ok, Result} | {error, Error}
</code></pre>
<br />

Handle replication.

<a name="replicate-4"></a>

### replicate/4 ###

`replicate(Server, Source, Target, Options) -> any()`

handle Replication. Allows to pass options with source and
target.  Options is a Json object.
ex:

```
  Options = [{<<"create_target">>, true}]}
  couchbeam:replicate(S, "testdb", "testdb2", Options).
```

<a name="save_doc-2"></a>

### save_doc/2 ###

`save_doc(Db, Doc) -> any()`

Equivalent to [`save_doc(Db, Doc, [])`](#save_doc-3).

save a document

<a name="save_doc-3"></a>

### save_doc/3 ###

<pre><code>
save_doc(Db::<a href="#type-db">db()</a>, Doc, Options::list()) -&gt; {ok, Doc1} | {error, Error}
</code></pre>
<br />

save a *document
A document is a Json object like this one:

```
          {[
           {<<"_id">>, <<"myid">>},
           {<<"title">>, <<"test">>}
       ]}
```

Options are arguments passed to the request. This function return a
new document with last revision and a docid. If _id isn't specified in
document it will be created. Id is created by extracting an uuid from
the couchdb node.

<a name="save_doc-4"></a>

### save_doc/4 ###

<pre><code>
save_doc(Db::<a href="#type-db">db()</a>, Doc::<a href="#type-doc">doc()</a>, Atts::<a href="#type-mp_attachments">mp_attachments()</a>, Options::list()) -&gt; {ok, <a href="#type-doc">doc()</a>} | {error, term()}
</code></pre>
<br />

save a *document with all its attacjments
A document is a Json object like this one:

```
          {[
           {<<"_id">>, <<"myid">>},
           {<<"title">>, <<"test">>}
       ]}
```

Options are arguments passed to the request. This function return a
new document with last revision and a docid. If _id isn't specified in
document it will be created. Id is created by extracting an uuid from
the couchdb node.

If the attachments is not empty, the doc will be sent as multipart.
Attachments are passed as a list of the following tuples:

- `{Name :: binary(), Bin :: binary()}`
- `{Name :: binary(), Bin :: binary(), Encoding :: binary()}`
- `{ Name :: binary(), Bin :: binary(), Type :: binary(), Encoding :: binary()}`
- `{ Name :: binary(), {file, Path ::  string()}}`
- `{ Name :: binary(), {file, Path ::  string()}, Encoding :: binary()}`
- `{ Name :: binary(), Fun :: fun(), Length :: integer()}`
- `{ Name :: binary(), Fun :: fun(), Length :: integer(), Encoding :: binary()}`
- `{Name :: binary(), Fun :: fun(), Length :: integer(), Type :: binary(), Encoding :: binary()}`
- `{ Name :: binary(), {Fun :: fun(), Acc :: any()}, Length :: integer()}`
- `{ Name :: binary(), {Fun :: fun(), Acc :: any()}, Length :: integer(), Encoding :: binary()}`
- `{ Name :: binary(), {Fun :: fun(), Acc :: any()}, Length :: integer(), Type :: binary(), Encoding :: binary()}.`

where `Type` is the content-type of the attachments (detected in other
case) and `Encoding` the encoding of the attachments:
`<<"identity">>` if normal or `<<"gzip">>` if the attachments is
gzipped.

<a name="save_docs-2"></a>

### save_docs/2 ###

`save_docs(Db, Docs) -> any()`

Equivalent to [`save_docs(Db, Docs, [])`](#save_docs-3).

save a list of documents

<a name="save_docs-3"></a>

### save_docs/3 ###

<pre><code>
save_docs(Db::<a href="#type-db">db()</a>, Docs::list(), Options::list()) -&gt; {ok, Result} | {error, Error}
</code></pre>
<br />

save a list of documents

<a name="send_attachment-2"></a>

### send_attachment/2 ###

`send_attachment(Ref, Msg) -> any()`

send an attachment chunk
Msg could be Data, eof to stop sending.

<a name="server_connection-0"></a>

### server_connection/0 ###

`server_connection() -> any()`

Equivalent to [`server_connection("127.0.0.1", 5984, "", [], false)`](#server_connection-5).

Create a server for connectiong to a CouchDB node

<a name="server_connection-1"></a>

### server_connection/1 ###

`server_connection(URL) -> any()`

<a name="server_connection-2"></a>

### server_connection/2 ###

`server_connection(URL, Options) -> any()`

Equivalent to [`server_connection(Host, Port, "", [])`](#server_connection-4).

Create a server for connectiong to a CouchDB node

<a name="server_connection-4"></a>

### server_connection/4 ###

<pre><code>
server_connection(Host::string(), Port::non_neg_integer(), Prefix::string(), OptionsList::list()) -&gt; Server::<a href="#type-server">server()</a>
</code></pre>
<br />

Create a server for connectiong to a CouchDB node

Connections are made to:

```
          http://Host:PortPrefix
```

If ssl is set https is used.

For a description of SSL Options, look in the [ssl](http://www.erlang.org/doc/apps/ssl/index.html) manpage.

<a name="server_info-1"></a>

### server_info/1 ###

<pre><code>
server_info(Server::<a href="#type-server">server()</a>) -&gt; {ok, iolist()}
</code></pre>
<br />

Get Information from the server

<a name="stream_attachment-1"></a>

### stream_attachment/1 ###

<pre><code>
stream_attachment(Ref::atom()) -&gt; {ok, binary()} | done | {error, term()}
</code></pre>
<br />

fetch an attachment chunk.
Use this function when you pass the `stream` option to the
`couchbeam:fetch_attachment/4` function.
This function return the following response:



<dt>done</dt>




<dd>You got all the attachment</dd>




<dt>{ok, binary()}</dt>




<dd>Part of the attachment</dd>




<dt>{error, term()}</dt>




<dd>n error occurred</dd>




<a name="stream_doc-1"></a>

### stream_doc/1 ###

<pre><code>
stream_doc(X1::<a href="#type-doc_stream">doc_stream()</a>) -&gt; {doc, <a href="#type-doc">doc()</a>} | {att, Name::binary(), <a href="#type-doc_stream">doc_stream()</a>} | {att_body, Name::binary(), Chunk::binary(), <a href="#type-doc_stream">doc_stream()</a>} | {att_eof, Name::binary(), <a href="#type-doc_stream">doc_stream()</a>} | eof | {error, term()}
</code></pre>
<br />

stream the multipart response of the doc API. Use this function
when you get `{ok, {multipart, State}}` from the function
`couchbeam:open_doc/3`.

<a name="view_cleanup-1"></a>

### view_cleanup/1 ###

`view_cleanup(Db) -> any()`

