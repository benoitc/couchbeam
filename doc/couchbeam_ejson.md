

# Module couchbeam_ejson #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>decode a binary to an EJSON term.</td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>encode an erlang term to JSON.</td></tr><tr><td valign="top"><a href="#post_decode-1">post_decode/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(D::binary()) -&gt; <a href="#type-ejson">ejson()</a>
</code></pre>
<br />

decode a binary to an EJSON term. Throw an exception if there is
any error.

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(D::<a href="#type-ejson">ejson()</a>) -&gt; binary()
</code></pre>
<br />

encode an erlang term to JSON. Throw an exception if there is
any error.

<a name="post_decode-1"></a>

### post_decode/1 ###

`post_decode(Rest) -> any()`

