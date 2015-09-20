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

-export([ping/1, request/2]).

-include("coap.hrl").

ping(Uri) ->
    {PeerIP, PortNo, _Path} = resolve_uri(Uri),
    channel_apply({PeerIP, PortNo},
        fun(Channel) ->
            {ok, Ref} = coap_channel:ping(Channel),
            case await_response(Channel, [], Ref) of
                {error, reset} -> ok;
                _Else -> error
            end
        end).

request(Method, Uri) ->
    {PeerIP, PortNo, Path} = resolve_uri(Uri),
    channel_apply({PeerIP, PortNo},
        fun(Channel) ->
            ROpt = uri_path(Path, []),
            {ok, Ref} = coap_channel:send(Channel,
                coap_message:request(con, Method, <<>>, ROpt)),
            await_response(Channel, ROpt, Ref)
        end).

uri_path([], Acc) ->
    Acc;
uri_path([$/], Acc) ->
    Acc;
uri_path([$/ | Path], Acc) ->
    [{uri_path, binary:split(list_to_binary(Path), [<<$/>>], [global])}|Acc].


await_response(Channel, ROpt, Ref) ->
    await_response(Channel, ROpt, Ref, <<>>).

await_response(Channel, ROpt, Ref, Resource) ->
    receive
        {coap_response, _ChId, Channel, Ref, #coap_message{method=Method, payload= <<>>}} ->
            Method;
        {coap_response, _ChId, Channel, Ref, #coap_message{method={ok, Code}, options=Options, payload=Data}} ->
            case proplists:get_value(block2, Options) of
                {Num, true, Size} ->
                    % more blocks follow, ask for more
                    {ok, Ref2} = coap_channel:send(Channel,
                        coap_message:request(con, get, <<>>, [{block2, {Num+1, false, Size}}|ROpt])),
                    await_response(Channel, ROpt, Ref2, <<Resource/binary, Data/binary>>);
                _Else ->
                    % not segmented
                    {ok, Code, #coap_resource{
                        etag = proplists:get_value(etag, Options),
                        format = proplists:get_value(content_format, Options),
                        content = <<Resource/binary, Data/binary>>}}
            end;
        {coap_error, _ChId, Channel, Ref, reset} ->
            {error, reset}
    end.


resolve_uri(Uri) ->
    {ok, {_Scheme, _UserInfo, Host, PortNo, Path, _Query}} =
        http_uri:parse(Uri, [{scheme_defaults, [{coap, 5683}]}]),
    {ok, PeerIP} = inet:getaddr(Host, inet),
    {PeerIP, PortNo, Path}.

channel_apply(ChId, Fun) ->
    {ok, Sock} = coap_udp_socket:start_link(),
    {ok, Channel} = coap_udp_socket:get_channel(Sock, ChId),
    % send and receive
    Res = apply(Fun, [Channel]),
    % terminate the processes
    coap_channel:close(Channel),
    coap_udp_socket:close(Sock),
    Res.

% end of file
