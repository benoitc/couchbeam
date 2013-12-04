-module(couchbeam_view_sup).

 -behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
    %% define a stream spec
    Stream = {coucbeam_view_stream, {coucbeam_view_stream, start_link, []},
              transient, 5000, worker, [coucbeam_view_stream]},

    %% start table to keep async streams ref
    ets:new(view_streams, [set, public, named_table]),

    {ok, {{simple_one_for_one, 10, 10}, [Stream]}}.
