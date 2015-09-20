%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% convenience functions for message construction
-module(coap_message).

-export([request/2, request/3, request/4, response/1, response/2, response/3]).
-export([set/3, set_payload/2, set_resource/3]).

-include("coap.hrl").

-define(MAX_BLOCK_SIZE, 1024).

request(Type, Method) ->
    request(Type, Method, <<>>, []).

request(Type, Method, Payload) ->
    request(Type, Method, Payload, []).

request(Type, Method, Payload, Options) ->
    #coap_message{type=Type, method=Method, payload=Payload, options=Options}.


response(Request=#coap_message{type=non}) ->
    #coap_message{
        type=non,
        token=Request#coap_message.token
    };
response(Request=#coap_message{type=con}) ->
    #coap_message{
        type=ack,
        id=Request#coap_message.id,
        token=Request#coap_message.token
    }.

response(Method, Request) ->
    set_method(Method,
        response(Request)).

response(Method, Payload, Request) ->
    set_method(Method,
        set_payload(Payload,
            response(Request))).


set(_Option, undefined, Msg) ->
    Msg;
set(Option, Value, Msg=#coap_message{options=Options}) ->
    Msg#coap_message{
        options=[{Option, Value}|Options]
    }.

set_method(Method, Msg) ->
    Msg#coap_message{
        method=Method
    }.

set_payload(Payload=#coap_resource{}, Msg) ->
    set_resource(Payload, undefined, Msg);
set_payload(Payload, Msg) when is_binary(Payload) ->
    Msg#coap_message{
        payload=Payload
    };
set_payload(Payload, Msg) when is_list(Payload) ->
    Msg#coap_message{
        payload=list_to_binary(Payload)
    }.

% segmentation not requested and not required
set_resource(#coap_resource{etag=ETag, format=Format, content=Content}, undefined, Msg)
        when byte_size(Content) =< ?MAX_BLOCK_SIZE ->
    set(etag, [ETag],
        set(content_format, Format,
            set_payload(Content, Msg)));
% segmentation not requested, but required (late negotiation)
set_resource(Resource, undefined, Msg) ->
    set_resource(Resource, {0, 0, ?MAX_BLOCK_SIZE}, Msg);
% segmentation requested (early negotiation)
set_resource(#coap_resource{etag=ETag, format=Format, content=Content}, Block, Msg) ->
    set(etag, [ETag],
        set(content_format, Format,
            set_payload_block(Content, Block, Msg))).

set_payload_block(Content, {Num, _, Size}, Msg) when byte_size(Content) > (Num+1)*Size ->
    set(block2, {Num, true, Size},
        set_payload(binary:part(Content, Num*Size, Size), Msg));
set_payload_block(Content, {Num, _, Size}, Msg) ->
    set(block2, {Num, false, Size},
        set_payload(binary:part(Content, Num*Size, byte_size(Content)-Num*Size), Msg)).

% end of file
