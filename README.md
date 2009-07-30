## README

**couchbeam** is a simple erlang CouchDB framework. couchbeam provides you a full featured and easy client to access and manage multiple couchdb Nodes.

## Features

* A full client in sync with latest CouchDB version
* Possible to use it as a simple standalone client (without starting application and such).
* It follows OTP principles if you want
* No dependancies. Http client use gen_recv and allow full streaming of attachments.
* Views can be retrieved while they coming (See example parse_incoming_view escript).
* Attachements are streamed and can be save on disk (or whatever you want) while they coming.
* Authentification support.


## Basic Standalone Usage 

1) Create a database :

    1> couchbeam:create_db({"127.0.0.1", 5984}, "somedb")

2) Save a doc, fetch it, edit it and view it

    2> Doc = {[{<<"field">>, <<"value">>}]},
    2> couchbeam:save_doc({"127.0.0.1", 5984}, "somedb", Doc).
    {ok,{[{<<"id">>,<<"eaff88948dfc860690d4460c51916dad">>},
          {<<"rev">>,<<"1-fe46b1a37e32aa544edb754885c0864b">>}]}}
          
    3> Doc1 = couchbeam:open_doc({"127.0.0.1", 5984}, "somedb", "eaff88948dfc860690d4460c51916dad").
    {[{<<"_id">>,<<"eaff88948dfc860690d4460c51916dad">>},
      {<<"_rev">>,<<"1-fe46b1a37e32aa544edb754885c0864b">>},
      {<<"field">>,<<"value">>}]}
      
    4> Doc2 = couchbeam:extend({<<"type">>, <<"test">>}, Doc1).
    {[{<<"_id">>,<<"eaff88948dfc860690d4460c51916dad">>},
      {<<"_rev">>,<<"1-fe46b1a37e32aa544edb754885c0864b">>},
      {<<"field">>,<<"value">>},
      {<<"type">>,<<"test">>}]}
      
    5> couchbeam:save_doc({"127.0.0.1", 5984}, "somedb", Doc2),
    5> DesignDoc = {[
    5>         {<<"_id">>, <<"_design/test">>},
    5>         {<<"language">>,<<"javascript">>},
    5>         {<<"views">>,
    5>             {[{<<"test">>,
    5>                 {[{<<"map">>,
    5>                     <<"function (doc) {\n if (doc.type == \"test\") {\n emit(doc._id, doc);\n}\n}">>
    5>                 }]}
    5>             }]}
    5>         }]},
    5> couchbeam:save_doc({"127.0.0.1", 5984}, "somedb", DesignDoc),
    5> couchbeam:query_view({"127.0.0.1", 5984}, "somedb", "test", "test").
    {[{<<"total_rows">>,1},
      {<<"offset">>,0},
      {<<"rows">>,
       [{[{<<"id">>,<<"eaff88948dfc860690d4460c51916dad">>},
          {<<"key">>,<<"eaff88948dfc860690d4460c51916dad">>},
          {<<"value">>,
           {[{<<"_id">>,<<"eaff88948dfc860690d4460c51916dad">>},
             {<<"_rev">>,<<"2-bb0dcac6a6c8987dad5b2dc2536b6b2e">>},
             {<<"field">>,<<"value">>},
             {<<"type">>,<<"test">>}]}}]}]}]}
             
3) Connect to a password protected CouchDB Node :

   1> couchbeam:create_db({"127.0.0.1", 5984, "admin", "testpassword"}, "mydb").
   
   
## Using as an OTP application :        

	1> application:start(crypto),
	1> couchbeam:start().
	ok
	
2) Add a connection to a node
 
	2> couchbeam:open_connection({"mynode", {"127.0.0.1", 5984}}),

`mynode` will be the name of connection you will use. Then you set the hostname and port of the CouchDB node you want to use. You can setup all the connections you need. It allow you to setup one connection. Later will be added possibility of load balancing. There is a `default` node to 127.0.0.1:5984 created when application starting. 

3) Play 

Get info of mynode server :

	3> couchbeam:server_info(mynode).
	[{<<"couchdb">>,<<"Welcome">>},
 	{<<"version">>,<<"0.10.0a">>}]	

Create a db :

	4> couchbeam:create_db(mynode, "couchbeam_testdb").
	ok
	
Create a doc :

	5> couchbeam:save_doc(mynode, "couchbeam_testdb", {[{<<"somefield">>, <<"somevalue">>}]}).
	{ok,{[{<<"id">>,<<"1dfa7d290f555857762a4491c27705b0">>},
	     {<<"rev">>,<<"1-823bb845c558d9bacac274f54ea91399">>}]}}
	
Update doc :

	6> couchbeam:save_doc(mynode, "couchbeam_testdb", {[{<<"_id">>, <<"1dfa7d290f555857762a4491c27705b0">>}, 		{<<"_rev">>, <<"1-823bb845c558d9bacac274f54ea91399">>}, {<<"somefield">>, <<"changedvalue">>}]}).
	{ok,{[{<<"id">>,<<"1dfa7d290f555857762a4491c27705b0">>},
	     {<<"rev">>,<<"2-89ede0192cf6beac8013fc229ff9eca7">>}]}}
	
Get doc :
	
	7> couchbeam:get_doc(mynode, "couchbeam_testdb", "1dfa7d290f555857762a4491c27705b0").
	{[{<<"_id">>,<<"1dfa7d290f555857762a4491c27705b0">>},
	 {<<"_rev">>,<<"2-89ede0192cf6beac8013fc229ff9eca7">>},
	 {<<"somefield">>,<<"changedvalue">>}]}
	

