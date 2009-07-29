## README

ecouchdbkit is a simple erlang CouchDB framework. ecouchdbkit provides you a full featured and easy client to access and manage multiple couchdb Nodes.

## Features

* a full client in sync with latest CouchDB version
* it follows OTP principles
* no dependancies. Http client use gen_recv and will allow full streaming of attachments


## Basic Usage 

1) Start application

	1> application:start(crypto),
	1> ecouchdbkit:start().
	ok
	
2) Add a connection to a node
 
	2> ecouchdbkit:open_connection({"mynode", {"127.0.0.1", 5984}}),

`mynode` will be the name of connection you will use. Then you set the hostname and port of the CouchDB node you want to use. You can setup all the connections you need. It allow you to setup one connection. Later will be added possibility of load balancing. There is a `default` node to 127.0.0.1:5984 created when application starting. 

3) Play 

Get info of mynode server :

	3> ecouchdbkit:server_info(mynode).
	[{<<"couchdb">>,<<"Welcome">>},
 	{<<"version">>,<<"0.10.0a">>}]	

Create a db :

	4> ecouchdbkit:create_db(mynode, "ecouchdbkit_testdb").
	ok
	
Create a doc :

	5> ecouchdbkit:save_doc(mynode, "ecouchdbkit_testdb", {[{<<"somefield">>, <<"somevalue">>}]}).
	{ok,{[{<<"id">>,<<"1dfa7d290f555857762a4491c27705b0">>},
	     {<<"rev">>,<<"1-823bb845c558d9bacac274f54ea91399">>}]}}
	
Update doc :

	6> ecouchdbkit:save_doc(mynode, "ecouchdbkit_testdb", {[{<<"_id">>, <<"1dfa7d290f555857762a4491c27705b0">>}, 		{<<"_rev">>, <<"1-823bb845c558d9bacac274f54ea91399">>}, {<<"somefield">>, <<"changedvalue">>}]}).
	{ok,{[{<<"id">>,<<"1dfa7d290f555857762a4491c27705b0">>},
	     {<<"rev">>,<<"2-89ede0192cf6beac8013fc229ff9eca7">>}]}}
	
Get doc :
	
	7> ecouchdbkit:get_doc(mynode, "ecouchdbkit_testdb", "1dfa7d290f555857762a4491c27705b0").
	{[{<<"_id">>,<<"1dfa7d290f555857762a4491c27705b0">>},
	 {<<"_rev">>,<<"2-89ede0192cf6beac8013fc229ff9eca7">>},
	 {<<"somefield">>,<<"changedvalue">>}]}
	
## Todo

- add attachments support
- add authentification management
- documentation
- some helpers to create a doc, extend a doc, 
- retrieve a view via a folding function...
	

