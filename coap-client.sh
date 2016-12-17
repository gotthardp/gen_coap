#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin examples/ebin

main(Params) ->
    sample_client:start(Params).
