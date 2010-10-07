-module(test_util).

-export([start_client/0]).

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
    process_flag(trap_exit, true),
    couchbeam:start(). 

