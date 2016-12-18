#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin examples/ebin

main(Params) ->
    ssl:start(),
    sample_client:start(Params).
