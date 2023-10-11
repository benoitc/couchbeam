

# Module gen_changes #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

gen_changes CouchDB continuous changes consumer behavior
This behaviour allows you to create easily a server that consume
Couchdb continuous changes.

__This module defines the `gen_changes` behaviour.__<br /> Required callback functions: `init/1`, `handle_change/2`, `handle_call/3`, `handle_cast/2`, `handle_info/2`, `terminate/2`.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#behaviour_info-1">behaviour_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#call-2">call/2</a></td><td></td></tr><tr><td valign="top"><a href="#call-3">call/3</a></td><td></td></tr><tr><td valign="top"><a href="#cast-2">cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_seq-1">get_seq/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td>create a gen_changes process as part of a supervision tree.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="behaviour_info-1"></a>

### behaviour_info/1 ###

`behaviour_info(X1) -> any()`

<a name="call-2"></a>

### call/2 ###

`call(Name, Request) -> any()`

<a name="call-3"></a>

### call/3 ###

`call(Name, Request, Timeout) -> any()`

<a name="cast-2"></a>

### cast/2 ###

`cast(Dest, Request) -> any()`

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVersion, State, Extra) -> any()`

<a name="get_seq-1"></a>

### get_seq/1 ###

`get_seq(Pid) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="start_link-4"></a>

### start_link/4 ###

<pre><code>
start_link(Module, Db::<a href="#type-db">db()</a>, Options::<a href="#type-changesoptions">changesoptions()</a>, InitArgs::list()) -&gt; term()
</code></pre>

<ul class="definitions"><li><code><a name="type-changesoptions">changesoptions()</a> = [<a href="#type-changeoption">changeoption()</a>]</code></li><li><code><a name="type-changeoption">changeoption()</a> = {include_docs, string()} | {filter, string()} | {since, integer() | string()} | {heartbeat, string() | boolean()}</code></li></ul>

create a gen_changes process as part of a supervision tree.
The function should be called, directly or indirectly, by the supervisor.

<a name="stop-1"></a>

### stop/1 ###

`stop(Pid) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, Gen_changes_state) -> any()`

