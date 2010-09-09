%% @author Benoît Chesneau <benoitc@e-engura.org>
%% @copyright 2009 Benoît Chesneau.
%%
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-type header() :: {string() | atom(), string()}.
-type headers() :: [header()].
%% In R13B bool() is now called boolean()
%% Uncomment if it's not compiling.
%% -type boolean() :: bool().

-record(couchdb_params, {
    host        = "127.0.0.1" :: string(),
    port        = 5984 :: integer(),
    ssl         = false :: boolean(),
    prefix      = "/" :: string(),
    username    = nil :: string(),
    password    = nil :: string(),
    name        = default :: term(),
    timeout     = infinity :: integer() | infinity,
    max_dbs_open = 100 :: integer(),
		headers     = [] :: headers()
}).

-record(server, {
    couchdb = #couchdb_params{},
    name,
    prefix,
    max_dbs_open,
    dbs_open=0,
    dbname_regexp,
    uuids_pid = undefined
}).


-record(db, {
    name,
    server,
    server_pid,
    couchdb = #couchdb_params{},
    base
}).

-record(old_view, {
    server,
    db,
    couchdb = #couchdb_params{},
    base,
    name,
    params=[],
    fetched=false,
    total_rows=0,
    offset=0,
    rows=[],
    meta=[],
    view_cache
}).

-record(view, {
    db,
    view_name,
    view_uri
}).

-record(change, {
    db,
    path,
    consumer_pid
}).

-record(uuids, {
    couchdb=#couchdb_params{},
    base,
    tid=undefined
}).

-record(couchbeam_manager, {
    conns=dict:new(),
    dbs=dict:new(),
    refs=dict:new()
}).
    
-define(USER_AGENT, "couchbeam/0.4.3").
