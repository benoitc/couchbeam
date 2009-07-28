#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(2),
    etap_can:loaded_ok(ecouchdbkit, "Module 'ecouchdbkit' loaded"),
    etap_can:can_ok(ecouchdbkit, server_info),
    etap:end_tests(),
    ok.