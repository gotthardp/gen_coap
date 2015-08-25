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
    {ok, {_Scheme, _UserInfo, Host, PortNo, _Path, _Query}} = http_uri:parse(Uri),
    {ok, PeerIP} = inet:getaddr(Host, inet),

    {ok, Sock} = coap_udp_socket:start_link(),
    {ok, Channel} = coap_udp_socket:connect(Sock, {PeerIP, PortNo}),

    {ok, Ref} = coap_request:send(Channel, Method, []),
    Res = await_response(Channel, Ref, <<>>),
    % terminate the processes
    coap_channel:close(Channel),
    coap_udp_socket:close(Sock),
    Res.

await_response(Channel, Ref, Resource) ->
    receive
        {coap_response, Channel, Ref, #coap_message{method={ok, Code}, payload=undefined}} ->
            {ok, Code};
        {coap_response, Channel, Ref, #coap_message{method={ok, Code}, options=Options, payload=Data}} ->
    	    case proplists:get_value(block2, Options) of
    	        [{Num, true, Size}] ->
    	            % more blocks follow, ask for more
    	            {ok, Ref2} = coap_request:send(Channel, get, [{block2, [{Num+1, false, Size}]}]),
    	            await_response(Channel, Ref2, <<Resource/binary, Data/binary>>);
    	        _Else ->
    	            % not segmented
    	            {ok, Code, <<Resource/binary, Data/binary>>}
    	    end
    end.

% end of file
