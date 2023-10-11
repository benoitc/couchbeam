

# Module couchbeam_util #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#binary_env-2">binary_env/2</a></td><td></td></tr><tr><td valign="top"><a href="#dbname-1">dbname/1</a></td><td></td></tr><tr><td valign="top"><a href="#deprecated-3">deprecated/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode_att_name-1">encode_att_name/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_docid-1">encode_docid/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_docid1-1">encode_docid1/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_docid_noop-1">encode_docid_noop/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_query-1">encode_query/1</a></td><td>Encode needed value of Query proplists in json.</td></tr><tr><td valign="top"><a href="#encode_query_value-2">encode_query_value/2</a></td><td>Encode value in JSON if needed depending on the key.</td></tr><tr><td valign="top"><a href="#force_param-3">force_param/3</a></td><td>replace a value in a proplist.</td></tr><tr><td valign="top"><a href="#get_app_env-2">get_app_env/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-2">get_value/2</a></td><td>emulate proplists:get_value/2,3 but use faster lists:keyfind/3.</td></tr><tr><td valign="top"><a href="#get_value-3">get_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#oauth_header-3">oauth_header/3</a></td><td></td></tr><tr><td valign="top"><a href="#parse_options-1">parse_options/1</a></td><td>make view options a list.</td></tr><tr><td valign="top"><a href="#parse_options-2">parse_options/2</a></td><td></td></tr><tr><td valign="top"><a href="#propmerge-3">propmerge/3</a></td><td>merge 2 proplists.</td></tr><tr><td valign="top"><a href="#propmerge1-2">propmerge1/2</a></td><td>Update a proplist with values of the second.</td></tr><tr><td valign="top"><a href="#proxy_header-3">proxy_header/3</a></td><td></td></tr><tr><td valign="top"><a href="#proxy_token-2">proxy_token/2</a></td><td></td></tr><tr><td valign="top"><a href="#shutdown_sync-1">shutdown_sync/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_app_deps-1">start_app_deps/1</a></td><td>Start depedent applications of App.</td></tr><tr><td valign="top"><a href="#to_atom-1">to_atom/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_integer-1">to_integer/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="binary_env-2"></a>

### binary_env/2 ###

`binary_env(Key, Default) -> any()`

<a name="dbname-1"></a>

### dbname/1 ###

`dbname(DbName) -> any()`

<a name="deprecated-3"></a>

### deprecated/3 ###

`deprecated(Old, New, When) -> any()`

<a name="encode_att_name-1"></a>

### encode_att_name/1 ###

`encode_att_name(Name) -> any()`

<a name="encode_docid-1"></a>

### encode_docid/1 ###

`encode_docid(DocId) -> any()`

<a name="encode_docid1-1"></a>

### encode_docid1/1 ###

`encode_docid1(DocId) -> any()`

<a name="encode_docid_noop-1"></a>

### encode_docid_noop/1 ###

`encode_docid_noop(DocId) -> any()`

<a name="encode_query-1"></a>

### encode_query/1 ###

`encode_query(QSL) -> any()`

Encode needed value of Query proplists in json

<a name="encode_query_value-2"></a>

### encode_query_value/2 ###

`encode_query_value(K, V) -> any()`

Encode value in JSON if needed depending on the key

<a name="force_param-3"></a>

### force_param/3 ###

`force_param(Key, Value, Options) -> any()`

replace a value in a proplist

<a name="get_app_env-2"></a>

### get_app_env/2 ###

`get_app_env(Env, Default) -> any()`

<a name="get_value-2"></a>

### get_value/2 ###

<pre><code>
get_value(Key::term(), Prop::[term()]) -&gt; term()
</code></pre>
<br />

emulate proplists:get_value/2,3 but use faster lists:keyfind/3

<a name="get_value-3"></a>

### get_value/3 ###

<pre><code>
get_value(Key::term(), Prop::[term()], Default::term()) -&gt; term()
</code></pre>
<br />

<a name="oauth_header-3"></a>

### oauth_header/3 ###

`oauth_header(Url, Action, OauthProps) -> any()`

<a name="parse_options-1"></a>

### parse_options/1 ###

`parse_options(Options) -> any()`

make view options a list

<a name="parse_options-2"></a>

### parse_options/2 ###

`parse_options(Rest, Acc) -> any()`

<a name="propmerge-3"></a>

### propmerge/3 ###

`propmerge(F, L1, L2) -> any()`

merge 2 proplists. All the Key - Value pairs from both proplists
are included in the new proplists. If a key occurs in both dictionaries
then Fun is called with the key and both values to return a new
value. This a wreapper around dict:merge

<a name="propmerge1-2"></a>

### propmerge1/2 ###

`propmerge1(L1, L2) -> any()`

Update a proplist with values of the second. In case the same
key is in 2 proplists, the value from the first are kept.

<a name="proxy_header-3"></a>

### proxy_header/3 ###

`proxy_header(UserName, Roles, Secret) -> any()`

<a name="proxy_token-2"></a>

### proxy_token/2 ###

`proxy_token(Secret, UserName) -> any()`

<a name="shutdown_sync-1"></a>

### shutdown_sync/1 ###

`shutdown_sync(Pid) -> any()`

<a name="start_app_deps-1"></a>

### start_app_deps/1 ###

<pre><code>
start_app_deps(App::atom()) -&gt; ok
</code></pre>
<br />

Start depedent applications of App.

<a name="to_atom-1"></a>

### to_atom/1 ###

`to_atom(V) -> any()`

<a name="to_binary-1"></a>

### to_binary/1 ###

`to_binary(V) -> any()`

<a name="to_integer-1"></a>

### to_integer/1 ###

`to_integer(V) -> any()`

<a name="to_list-1"></a>

### to_list/1 ###

`to_list(V) -> any()`

