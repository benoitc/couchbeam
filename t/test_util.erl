-module(test_util).

-export([start_client/0, start_apps/1]).

builddir() ->
    {file, Here} = code:is_loaded(?MODULE),
    filename:dirname(filename:join("..", filename:dirname(Here))).

init_code_path() ->
    code:add_pathz(filename:join([builddir(), "ebin"])),
    % add deps
    Paths = ["crypto", "sasl", "ibrowse", "mochiweb"],
    lists:foreach(fun(Name) ->
        code:add_pathz(filename:join([builddir(), "deps", Name, "ebin"]))
    end, Paths).

start_client() ->
    init_code_path(),
    case start_apps([crypto, public_key, sasl, ssl, ibrowse]) of
    ok ->
        process_flag(trap_exit, true),
        couchbeam:start();
    {error, Reason} ->
        {error, Reason}
    end. 

start_apps([]) ->
    ok;
start_apps([App|Rest]) ->
    case application:start(App) of
    ok ->
       start_apps(Rest);
    {error, {already_started, App}} ->
       start_apps(Rest);
    {error, _Reason} when App =:= public_key ->
       % ignore on R12B5
       start_apps(Rest);
    {error, _Reason} ->
       {error, {app_would_not_start, App}}
    end.
