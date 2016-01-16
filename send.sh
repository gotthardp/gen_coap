#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin

main([]) ->
    query("coap://coap.me:5683").
%    query("coap://127.0.0.1:5683").
    
query(Uri) ->
    {ok, content, Data} = coap_client:request(get, Uri),
    Res = core_link:decode(Data),
    io:format("~p~n", [Res]).

% end of file
            