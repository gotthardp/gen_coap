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
-export([new/3, response/1, response/2, response/3, set/3, set_payload/2]).

-include("coap.hrl").

new(Type, Token, Method) ->
    #coap_message{
        type=Type,
        token=Token,
        method=Method
    }.

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
        options=[{Option, [Value]}|Options]
    }.

set_method(Method, Msg) ->
    Msg#coap_message{
        method=Method
    }.

set_payload(Payload, Msg) when is_binary(Payload) ->
    Msg#coap_message{
        payload=Payload
    };
set_payload(Payload, Msg) when is_list(Payload) ->
    Msg#coap_message{
        payload=list_to_binary(Payload)
    }.

% end of file
