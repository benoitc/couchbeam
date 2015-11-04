

# Module couchbeam_doc #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-key_val">key_val()</a> ###


<pre><code>
key_val() = <a href="#type-lis">lis()</a> | binary()
</code></pre>




### <a name="type-property">property()</a> ###


<pre><code>
property() = <a href="#type-json_obj">json_obj()</a> | tuple()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete_value-2">delete_value/2</a></td><td>Deletes all entries associated with Key in json object.</td></tr><tr><td valign="top"><a href="#extend-2">extend/2</a></td><td>extend a jsonobject by a property, list of property or another jsonobject.</td></tr><tr><td valign="top"><a href="#extend-3">extend/3</a></td><td>extend a jsonobject by key, value.</td></tr><tr><td valign="top"><a href="#get_id-1">get_id/1</a></td><td>get document id.</td></tr><tr><td valign="top"><a href="#get_idrev-1">get_idrev/1</a></td><td>get  a tuple containing docucment id and revision.</td></tr><tr><td valign="top"><a href="#get_rev-1">get_rev/1</a></td><td>get document revision.</td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td>Returns the value of a simple key/value property in json object
Equivalent to get_value(Key, JsonObj, undefined).</td></tr><tr><td valign="top"><a href="#get_value-3">get_value/3</a></td><td>Returns the value of a simple key/value property in json object
function from erlang_couchdb.</td></tr><tr><td valign="top"><a href="#is_saved-1">is_saved/1</a></td><td>If document have been saved (revision is defined) return true,
else, return false.</td></tr><tr><td valign="top"><a href="#set_value-3">set_value/3</a></td><td>set a value for a key in jsonobj.</td></tr><tr><td valign="top"><a href="#take_value-2">take_value/2</a></td><td>Returns the value of a simple key/value property in json object and deletes
it form json object
Equivalent to take_value(Key, JsonObj, undefined).</td></tr><tr><td valign="top"><a href="#take_value-3">take_value/3</a></td><td>Returns the value of a simple key/value property in json object and deletes
it from json object.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete_value-2"></a>

### delete_value/2 ###

<pre><code>
delete_value(Key::<a href="#type-key_val">key_val()</a>, JsonObj::<a href="#type-json_obj">json_obj()</a>) -&gt; <a href="#type-json_obj">json_obj()</a>
</code></pre>
<br />

Deletes all entries associated with Key in json object.

<a name="extend-2"></a>

### extend/2 ###

<pre><code>
extend(Prop::<a href="#type-property">property()</a>, JsonObj::<a href="#type-json_obj">json_obj()</a>) -&gt; <a href="#type-json_obj">json_obj()</a>
</code></pre>
<br />

extend a jsonobject by a property, list of property or another jsonobject

<a name="extend-3"></a>

### extend/3 ###

<pre><code>
extend(Key::binary(), Value::<a href="#type-json_term">json_term()</a>, JsonObj::<a href="#type-json_obj">json_obj()</a>) -&gt; <a href="#type-json_obj">json_obj()</a>
</code></pre>
<br />

extend a jsonobject by key, value

<a name="get_id-1"></a>

### get_id/1 ###

<pre><code>
get_id(Doc::<a href="#type-json_obj">json_obj()</a>) -&gt; binary()
</code></pre>
<br />

get document id.

<a name="get_idrev-1"></a>

### get_idrev/1 ###

<pre><code>
get_idrev(Doc::<a href="#type-json_obj">json_obj()</a>) -&gt; {DocId, DocRev}
</code></pre>
<br />

get  a tuple containing docucment id and revision.

<a name="get_rev-1"></a>

### get_rev/1 ###

<pre><code>
get_rev(Doc::<a href="#type-json_obj">json_obj()</a>) -&gt; binary()
</code></pre>
<br />

get document revision.

<a name="get_value-2"></a>

### get_value/2 ###

<pre><code>
get_value(Key::<a href="#type-key_val">key_val()</a>, JsonObj::<a href="#type-json_obj">json_obj()</a>) -&gt; term()
</code></pre>
<br />

Returns the value of a simple key/value property in json object
Equivalent to get_value(Key, JsonObj, undefined).

<a name="get_value-3"></a>

### get_value/3 ###

<pre><code>
get_value(Key::<a href="#type-lis">lis()</a> | binary(), JsonObj::<a href="#type-json_obj">json_obj()</a>, Default::term()) -&gt; term()
</code></pre>
<br />

Returns the value of a simple key/value property in json object
function from erlang_couchdb

<a name="is_saved-1"></a>

### is_saved/1 ###

<pre><code>
is_saved(Doc::<a href="#type-json_obj">json_obj()</a>) -&gt; boolean()
</code></pre>
<br />

If document have been saved (revision is defined) return true,
else, return false.

<a name="set_value-3"></a>

### set_value/3 ###

<pre><code>
set_value(Key::<a href="#type-key_val">key_val()</a>, Value::term(), JsonObj::<a href="#type-json_obj">json_obj()</a>) -&gt; term()
</code></pre>
<br />

set a value for a key in jsonobj. If key exists it will be updated.

<a name="take_value-2"></a>

### take_value/2 ###

<pre><code>
take_value(Key::<a href="#type-key_val">key_val()</a>, JsonObj::<a href="#type-json_obj">json_obj()</a>) -&gt; {term(), <a href="#type-json_obj">json_obj()</a>}
</code></pre>
<br />

Returns the value of a simple key/value property in json object and deletes
it form json object
Equivalent to take_value(Key, JsonObj, undefined).

<a name="take_value-3"></a>

### take_value/3 ###

<pre><code>
take_value(Key::<a href="#type-key_val">key_val()</a> | binary(), JsonObj::<a href="#type-json_obj">json_obj()</a>, Default::term()) -&gt; {term(), <a href="#type-json_obj">json_obj()</a>}
</code></pre>
<br />

Returns the value of a simple key/value property in json object and deletes
it from json object

