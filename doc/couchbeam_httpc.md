

# Module couchbeam_httpc #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#db_request-5">db_request/5</a></td><td></td></tr><tr><td valign="top"><a href="#db_request-6">db_request/6</a></td><td></td></tr><tr><td valign="top"><a href="#db_resp-2">db_resp/2</a></td><td></td></tr><tr><td valign="top"><a href="#db_url-1">db_url/1</a></td><td></td></tr><tr><td valign="top"><a href="#doc_url-2">doc_url/2</a></td><td></td></tr><tr><td valign="top"><a href="#json_body-1">json_body/1</a></td><td></td></tr><tr><td valign="top"><a href="#make_headers-4">make_headers/4</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_oauth_header-4">maybe_oauth_header/4</a></td><td></td></tr><tr><td valign="top"><a href="#request-5">request/5</a></td><td></td></tr><tr><td valign="top"><a href="#server_url-1">server_url/1</a></td><td>Asemble the server URL for the given client.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="db_request-5"></a>

### db_request/5 ###

`db_request(Method, Url, Headers, Body, Options) -> any()`

<a name="db_request-6"></a>

### db_request/6 ###

`db_request(Method, Url, Headers, Body, Options, Expect) -> any()`

<a name="db_resp-2"></a>

### db_resp/2 ###

`db_resp(Resp, Expect) -> any()`

<a name="db_url-1"></a>

### db_url/1 ###

`db_url(Db) -> any()`

<a name="doc_url-2"></a>

### doc_url/2 ###

`doc_url(Db, DocId) -> any()`

<a name="json_body-1"></a>

### json_body/1 ###

`json_body(Ref) -> any()`

<a name="make_headers-4"></a>

### make_headers/4 ###

`make_headers(Method, Url, Headers, Options) -> any()`

<a name="maybe_oauth_header-4"></a>

### maybe_oauth_header/4 ###

`maybe_oauth_header(Method, Url, Headers, Options) -> any()`

<a name="request-5"></a>

### request/5 ###

`request(Method, Url, Headers, Body, Options) -> any()`

<a name="server_url-1"></a>

### server_url/1 ###

<pre><code>
server_url(Server::{Host, Port}) -&gt; iolist()
</code></pre>
<br />

Asemble the server URL for the given client

