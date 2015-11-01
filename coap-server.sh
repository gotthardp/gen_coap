#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin

main(_Params) ->
    sample_server:start(),
    receive stop->ok end.
