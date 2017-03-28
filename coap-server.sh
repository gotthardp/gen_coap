#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin _build/default/lib/gen_coap/ebin

main(_Params) ->
    sample_server:start(),
    receive stop->ok end.
