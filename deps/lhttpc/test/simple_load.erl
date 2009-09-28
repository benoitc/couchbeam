-module(simple_load).
-behaviour(gen_httpd).

-export([start_client/1, start_client/3]).
-export([client/2]).

-export([start_server/1]).
-export([init/2, handle_continue/5, handle_request/6, terminate/2]).

%%% Client part
start_client(Clients) ->
	start_client("localhost", 9999, Clients).

start_client(Host, Port, Clients) when Clients > 0 ->
	process_flag(trap_exit, true),
	{ok, Body} = file:read_file("tests/1M"),
	URL = "http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/static/1M",
	start(Clients, URL, Body, Clients).

start(0, _, _, No) ->
	wait_exit(No, []);
start(Clients, URL, Body, No) ->
	spawn_link(?MODULE, client, [URL, Body]),
	start(Clients - 1, URL, Body, No).

wait_exit(0, []) ->
	ok;
wait_exit(0, Errors) ->
	{error, Errors};
wait_exit(No, Errors) ->
	receive
		{'EXIT', _, normal} ->
			wait_exit(No - 1, Errors);
		{'EXIT', _, Reason} ->
			wait_exit(No - 1, [Reason | Errors])
	end.

client(URL, Body) ->
	case lhttpc:request(URL, "POST", [], Body, 60000) of
		{ok, {{200, _}, _, Body}} ->
			ok;
		Other ->
			exit({bad_result, Other})
	end.

%%% Server part
start_server(Port) ->
	SockOpts = [{backlog, 10000}],
	gen_httpd:start_link(?MODULE, nil, Port, 600000, SockOpts).

init(_, _) ->
	{ok, nil}.

handle_continue(_Method, _URI, _Vsn, _ReqHdrs, CBState) ->
	{continue, [], CBState}.

handle_request(_Method, "/static/1M", {1,1}, _, Entity, State) ->
	case Entity of
		{identity, Reader} ->
			case Reader(complete, 50000) of
				{ok, Body} ->
					{reply, 200, [], Body, State};
				{error, Reason} ->
					{reply, 500, [], io_lib:format("~p", [Reason]), State}
			end;
		_ ->
			{reply, 406, [], <<"No request body">>, State}
	end.

terminate(_, _) ->
	ok.
