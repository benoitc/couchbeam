#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(2),
    etap_can:loaded_ok(couchbeam, "Module 'couchbeam' loaded"),
    etap_can:can_ok(couchbeam, server_info),
    etap:end_tests(),
    ok.
