#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin _build/default/lib/gen_coap/ebin

main(Params) ->
    ssl:start(),
    sample_client:start(Params).
