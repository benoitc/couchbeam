%% @author Benoît Chesneau <benoitc@e-engura.org>
%% @copyright 2009 Benoît Chesneau.
%%
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.


-record(ecouchdbkit_srv,{
    ets_tid,
    nodes_tid}).

-record(couchdb_node, {
    host,
    port,
    username,
    password,
    ets_tid}).
    
-record(http_response, {version,
                        status,
                        phrase}).
    
-define(USER_AGENT, "ecouchdbkit/0.1").

-define(b2l(V), binary_to_list(V)).
-define(l2b(V), list_to_binary(V)).