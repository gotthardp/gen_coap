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

-export([ping/1, request/2, request/3, request/4, ack/2]).
-export([resolve_uri/1, await_response/5]).

-include("coap.hrl").

ping(Uri) ->
    {Scheme, ChId, _Path, _Query} = resolve_uri(Uri),
    channel_apply(Scheme, ChId,
        fun(Channel) ->
            {ok, Ref} = coap_channel:ping(Channel),
            case await_response(Channel, undefined, [], Ref, <<>>) of
                {error, reset} -> ok;
                _Else -> error
            end
        end).

request(Method, Uri) ->
    request(Method, Uri, #coap_content{}, []).

request(Method, Uri, Content) ->
    request(Method, Uri, Content, []).

request(Method, Uri, Content, Options) ->
    {Scheme, ChId, Path, Query} = resolve_uri(Uri),
    channel_apply(Scheme, ChId,
        fun(Channel) ->
            request_block(Channel, Method, [{uri_path, Path}, {uri_query, Query} | Options], Content)
        end).

request_block(Channel, Method, ROpt, Content) ->
    request_block(Channel, Method, ROpt, undefined, Content).

request_block(Channel, Method, ROpt, Block1, Content) ->
    {ok, Ref} = coap_channel:send(Channel,
        coap_message:set_content(Content, Block1,
            coap_message:request(con, Method, <<>>, ROpt))),
    await_response(Channel, Method, ROpt, Ref, Content).


await_response(Channel, Method, ROpt, Ref, Content) ->
    await_response(Channel, Method, ROpt, Ref, Content, <<>>).

await_response(Channel, Method, ROpt, Ref, Content, Fragment) ->
    receive
        {coap_response, _ChId, Channel, Ref, #coap_message{method={ok, continue}, options=Options}} ->
            case proplists:get_value(block1, Options) of
                {Num, true, Size} ->
                    request_block(Channel, Method, ROpt, {Num+1, false, Size}, Content)
            end;
        {coap_response, _ChId, Channel, Ref, Message=#coap_message{method={ok, Code}, options=Options, payload=Data}} ->
            case proplists:get_value(block2, Options) of
                {Num, true, Size} ->
                    % more blocks follow, ask for more
                    % no payload for requests with Block2 with NUM != 0
                    {ok, Ref2} = coap_channel:send(Channel,
                        coap_message:request(con, Method, <<>>, [{block2, {Num+1, false, Size}}|ROpt])),
                    await_response(Channel, Method, ROpt, Ref2, Content, <<Fragment/binary, Data/binary>>);
                _Else ->
                    % not segmented
                    return_response({ok, Code}, Message#coap_message{payload= <<Fragment/binary, Data/binary>>})
            end;
        {coap_response, _ChId, Channel, Ref, Message=#coap_message{method=Code}} ->
            return_response(Code, Message);
        {coap_error, _ChId, Channel, Ref, reset} ->
            {error, reset}
    end.

return_response({ok, Code}, Message) ->
    {ok, Code, coap_message:get_content(Message)};
return_response({error, Code}, #coap_message{payload= <<>>}) ->
    {error, Code};
return_response({error, Code}, Message) ->
    {error, Code, coap_message:get_content(Message)}.

ack(Channel, Message) ->
    coap_channel:send(Channel,
        coap_message:ack(Message)).


resolve_uri(Uri) ->
    {ok, {Scheme, _UserInfo, Host, PortNo, Path, Query}} =
        http_uri:parse(Uri, [{scheme_defaults, [{coap, ?DEFAULT_COAP_PORT}, {coaps, ?DEFAULT_COAPS_PORT}]}]),
    {ok, PeerIP} = inet:getaddr(Host, inet),
    {Scheme, {PeerIP, PortNo}, split_path(Path), split_query(Query)}.

split_path([]) -> [];
split_path([$/]) -> [];
split_path([$/ | Path]) -> split_segments(Path, $/, []).

split_query([]) -> [];
split_query([$? | Path]) -> split_segments(Path, $&, []).

split_segments(Path, Char, Acc) ->
    case string:rchr(Path, Char) of
        0 ->
            [make_segment(Path) | Acc];
        N when N > 0 ->
            split_segments(string:substr(Path, 1, N-1), Char,
                [make_segment(string:substr(Path, N+1)) | Acc])
    end.

make_segment(Seg) ->
    list_to_binary(http_uri:decode(Seg)).

channel_apply(coap, ChId, Fun) ->
    {ok, Sock} = coap_udp_socket:start_link(),
    {ok, Channel} = coap_udp_socket:get_channel(Sock, ChId),
    % send and receive
    Res = apply(Fun, [Channel]),
    % terminate the processes
    coap_channel:close(Channel),
    coap_udp_socket:close(Sock),
    Res;

channel_apply(coaps, {Host, Port}, Fun) ->
    {ok, Sock, Channel} = coap_dtls_socket:connect(Host, Port),
    % send and receive
    Res = apply(Fun, [Channel]),
    % terminate the processes
    coap_channel:close(Channel),
    coap_dtls_socket:close(Sock),
    Res.

-include_lib("eunit/include/eunit.hrl").

% note that the options below must be sorted by the option numbers
resolver_test_()-> [
    ?_assertEqual({coap, {{127,0,0,1},?DEFAULT_COAP_PORT},[], []}, resolve_uri("coap://localhost")),
    ?_assertEqual({coap, {{127,0,0,1},1234},[], []}, resolve_uri("coap://localhost:1234")),
    ?_assertEqual({coap, {{127,0,0,1},?DEFAULT_COAP_PORT},[], []}, resolve_uri("coap://localhost/")),
    ?_assertEqual({coap, {{127,0,0,1},1234},[], []}, resolve_uri("coap://localhost:1234/")),
    ?_assertEqual({coaps, {{127,0,0,1},?DEFAULT_COAPS_PORT},[], []}, resolve_uri("coaps://localhost")),
    ?_assertEqual({coaps, {{127,0,0,1},1234},[], []}, resolve_uri("coaps://localhost:1234")),
    ?_assertEqual({coaps, {{127,0,0,1},?DEFAULT_COAPS_PORT},[], []}, resolve_uri("coaps://localhost/")),
    ?_assertEqual({coaps, {{127,0,0,1},1234},[], []}, resolve_uri("coaps://localhost:1234/")),
    ?_assertEqual({coap, {{127,0,0,1},?DEFAULT_COAP_PORT},[<<"/">>], []}, resolve_uri("coap://localhost/%2F")),
    % from RFC 7252, Section 6.3
    % the following three URIs are equivalent
    ?_assertEqual({coap, {{127,0,0,1},5683},[<<"~sensors">>, <<"temp.xml">>], []},
        resolve_uri("coap://localhost:5683/~sensors/temp.xml")),
    ?_assertEqual({coap, {{127,0,0,1},?DEFAULT_COAP_PORT},[<<"~sensors">>, <<"temp.xml">>], []},
        resolve_uri("coap://LOCALHOST/%7Esensors/temp.xml")),
    ?_assertEqual({coap, {{127,0,0,1},?DEFAULT_COAP_PORT},[<<"~sensors">>, <<"temp.xml">>], []},
        resolve_uri("coap://LOCALHOST/%7esensors/temp.xml")),
    % from RFC 7252, Appendix B
    ?_assertEqual({coap, {{127,0,0,1},61616},[<<>>, <<"/">>, <<>>, <<>>], [<<"//">>,<<"?&">>]},
        resolve_uri("coap://localhost:61616//%2F//?%2F%2F&?%26"))
    ].

% end of file
