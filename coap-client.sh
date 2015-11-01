#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin

main(Params) ->
    sample_client:start(Params).
