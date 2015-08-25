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
-export([content/2, content/3, response/1, response/2, response/3]).

-include("coap.hrl").

content(Type, Payload) ->
    content(Type, Payload, #coap_message{}).

content(Type, Payload, Msg) ->
    Msg#coap_message{
        method={ok, content},
        options=[{content_format, [Type]}],
        payload=Payload
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
    Res = response(Request),
    Res#coap_message{
        method=Method
    }.

response(Method, Payload, Request) ->
    Res = response(Request),
    Res#coap_message{
        method=Method,
        payload=Payload
    }.

% end of file
