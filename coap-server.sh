#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin examples/ebin

main(_Params) ->
    sample_server:start(),
    receive stop->ok end.
