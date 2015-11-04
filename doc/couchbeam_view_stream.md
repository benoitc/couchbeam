

# Module couchbeam_view_stream #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#collect_object-2">collect_object/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_event-2">handle_event/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#init_stream-5">init_stream/5</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_continue-1">maybe_continue/1</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_continue_decoding-1">maybe_continue_decoding/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr><tr><td valign="top"><a href="#system_code_change-4">system_code_change/4</a></td><td></td></tr><tr><td valign="top"><a href="#system_continue-3">system_continue/3</a></td><td></td></tr><tr><td valign="top"><a href="#system_terminate-4">system_terminate/4</a></td><td></td></tr><tr><td valign="top"><a href="#wait_rows-2">wait_rows/2</a></td><td></td></tr><tr><td valign="top"><a href="#wait_rows1-2">wait_rows1/2</a></td><td></td></tr><tr><td valign="top"><a href="#wait_val-2">wait_val/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="collect_object-2"></a>

### collect_object/2 ###

`collect_object(X1, X2) -> any()`

<a name="handle_event-2"></a>

### handle_event/2 ###

`handle_event(Event, St) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="init_stream-5"></a>

### init_stream/5 ###

`init_stream(Parent, Owner, StreamRef, Req, StreamOptions) -> any()`

<a name="maybe_continue-1"></a>

### maybe_continue/1 ###

`maybe_continue(State) -> any()`

<a name="maybe_continue_decoding-1"></a>

### maybe_continue_decoding/1 ###

`maybe_continue_decoding(Viewst) -> any()`

<a name="start_link-4"></a>

### start_link/4 ###

`start_link(Owner, StreamRef, X3, StreamOptions) -> any()`

<a name="system_code_change-4"></a>

### system_code_change/4 ###

`system_code_change(Misc, X2, X3, X4) -> any()`

<a name="system_continue-3"></a>

### system_continue/3 ###

`system_continue(X1, X2, X3) -> any()`

<a name="system_terminate-4"></a>

### system_terminate/4 ###

<pre><code>
system_terminate(Reason::any(), X2::term(), X3::term(), State::term()) -&gt; no_return()
</code></pre>
<br />

<a name="wait_rows-2"></a>

### wait_rows/2 ###

`wait_rows(X1, St) -> any()`

<a name="wait_rows1-2"></a>

### wait_rows1/2 ###

`wait_rows1(X1, X2) -> any()`

<a name="wait_val-2"></a>

### wait_val/2 ###

`wait_val(X1, X2) -> any()`

