

# Module couchbeam_uuids #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_uuids-2">get_uuids/2</a></td><td>Get a list of uuids from the server.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#random-0">random/0</a></td><td>return a random uuid.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Starts the couchbeam process linked to the calling process.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#utc_random-0">utc_random/0</a></td><td>return a random uuid based on time.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="get_uuids-2"></a>

### get_uuids/2 ###

<pre><code>
get_uuids(Server::<a href="#type-server">server()</a>, Count::integer()) -&gt; <a href="#type-lists">lists()</a>
</code></pre>
<br />

Get a list of uuids from the server

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="random-0"></a>

### random/0 ###

<pre><code>
random() -&gt; binary()
</code></pre>
<br />

return a random uuid

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, pid()}
</code></pre>
<br />

Starts the couchbeam process linked to the calling process. Usually
invoked by the supervisor couchbeam_sup

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="utc_random-0"></a>

### utc_random/0 ###

<pre><code>
utc_random() -&gt; binary()
</code></pre>
<br />

return a random uuid based on time

