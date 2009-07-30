## README

couchbeam is a simple erlang CouchDB framework. couchbeam provides you a full featured and easy client to access and manage multiple couchdb Nodes.

## Features

* a full client in sync with latest CouchDB version
* possible to use it as a simple standalone client (without starting application and such).
* it follows OTP principles if you want
* no dependancies. Http client use gen_recv and will allow full streaming of attachments


## Basic Standalone Usage 

1) Create a database :

    1> couchbeam:create_db({"127.0.0.1", 5984}, "somedb")

2) Save a doc and fetch it

    2> Doc = {[{<<"field">>, <<"value">>}]},
    2> couchbeam:save_doc({"127.0.0.1", 5984}, "somedb", Doc).
    {ok,{[{<<"id">>,<<"eaff88948dfc860690d4460c51916dad">>},
          {<<"rev">>,<<"1-fe46b1a37e32aa544edb754885c0864b">>}]}}
    3> Doc1 = couchbeam:open_doc({"127.0.0.1", 5984}, "somedb", "eaff88948dfc860690d4460c51916dad").
    {[{<<"_id">>,<<"eaff88948dfc860690d4460c51916dad">>},
      {<<"_rev">>,<<"1-fe46b1a37e32aa544edb754885c0864b">>},
      {<<"field">>,<<"value">>}]}
    
     
    
    
    
    


1) Start application

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
	
## Todo

- add attachments support
- add authentification management
- documentation
- some helpers to create a doc, extend a doc, 
- retrieve a view via a folding function...
	

