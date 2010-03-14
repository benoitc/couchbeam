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
    timeout     = infinity :: integer() | infinity
}).

-record(server_state, {
    couchdb = #couchdb_params{},
    prefix,
    name,
    dbs_by_name = undefined,
    dbs_by_pid = undefined,
    uuids_pid = undefined
}).


-record(db, {
    name,
    server,
    couchdb = #couchdb_params{},
    base
}).

-record(view, {
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
    
-define(USER_AGENT, "couchbeam/0.4.1").
