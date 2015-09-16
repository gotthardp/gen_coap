%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% convenience functions for building CoAP clients
-module(coap_client).

-export([request/2]).

-include("coap.hrl").

request(Method, Uri) ->
    {ok, {_Scheme, _UserInfo, Host, PortNo, Path, _Query}} =
        http_uri:parse(Uri, [{scheme_defaults, [{coap, 5683}]}]),
    {ok, PeerIP} = inet:getaddr(Host, inet),

    {ok, Sock} = coap_udp_socket:start_link(),
    {ok, Channel} = coap_udp_socket:get_channel(Sock, {PeerIP, PortNo}),

    {ok, Ref} = coap_request:send(Channel, con, Method, <<>>,
        uri_path(Path, [])),
    Res = await_response(Channel, Ref, <<>>),
    % terminate the processes
    coap_channel:close(Channel),
    coap_udp_socket:close(Sock),
    Res.

uri_path([], Acc) ->
    Acc;
uri_path([$/], Acc) ->
    Acc;
uri_path([$/ | Path], Acc) ->
    [{uri_path, binary:split(list_to_binary(Path), [<<$/>>], [global])}|Acc].

await_response(Channel, Ref, Resource) ->
    receive
        {coap_response, _ChId, Channel, Ref, #coap_message{method=Method, payload= <<>>}} ->
            Method;
        {coap_response, _ChId, Channel, Ref, #coap_message{method={ok, Code}, options=Options, payload=Data}} ->
            case proplists:get_value(block2, Options) of
                [{Num, true, Size}] ->
                    % more blocks follow, ask for more
                    {ok, Ref2} = coap_request:send(Channel, con, get, <<>>, [{block2, [{Num+1, false, Size}]}]),
                    await_response(Channel, Ref2, <<Resource/binary, Data/binary>>);
                _Else ->
                    % not segmented
                    {ok, Code, <<Resource/binary, Data/binary>>}
            end
    end.

% end of file
