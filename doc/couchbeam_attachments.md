

# Module couchbeam_attachments #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

This module contains utilities to manage attachments.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_inline-3">add_inline/3</a></td><td>add attachment  to a doc and encode it.</td></tr><tr><td valign="top"><a href="#add_inline-4">add_inline/4</a></td><td>add attachment  to a doc and encode it with ContentType fixed.</td></tr><tr><td valign="top"><a href="#add_stub-3">add_stub/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete_inline-2">delete_inline/2</a></td><td>delete an attachment record in doc.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_inline-3"></a>

### add_inline/3 ###

<pre><code>
add_inline(Doc::<a href="#type-json_obj">json_obj()</a>, Content::<a href="#type-attachment_content">attachment_content()</a>, AName::string()) -&gt; <a href="#type-json_obj">json_obj()</a>
</code></pre>
<br />

add attachment  to a doc and encode it. Give possibility to send attachments inline.

<a name="add_inline-4"></a>

### add_inline/4 ###

<pre><code>
add_inline(Doc::<a href="#type-json_obj">json_obj()</a>, Content::<a href="#type-attachment_content">attachment_content()</a>, AName::string(), ContentType::string()) -&gt; <a href="#type-json_obj">json_obj()</a>
</code></pre>
<br />

add attachment  to a doc and encode it with ContentType fixed.

<a name="add_stub-3"></a>

### add_stub/3 ###

`add_stub(Doc, Name, ContentType) -> any()`

<a name="delete_inline-2"></a>

### delete_inline/2 ###

<pre><code>
delete_inline(Doc::<a href="#type-json_obj">json_obj()</a>, AName::string()) -&gt; <a href="#type-json_obj">json_obj()</a>
</code></pre>
<br />

delete an attachment record in doc. This is different from delete_attachment
change is only applied in Doc object. Save_doc should be save to save changes.

