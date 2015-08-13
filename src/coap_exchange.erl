%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% convenience functions for exchange manipulation
-module(coap_exchange).

-include("coap.hrl").

-export([send_message/2, reply/3, reply/4, reply_content/4]).

reply(Source, Request, Method) ->
    reply(Source, Request, Method, <<>>).

reply({_Pid, Sender}, Request=#coap_message{type=non}, Method, Payload) ->
    Response = coap_message:response(Method, Payload, coap_message:non(Request)),
    coap_endpoint:start_exchange(Sender, Response);

reply({Pid, _Sender}, Request=#coap_message{type=con}, Method, Payload) ->
    Response = coap_message:response(Method, Payload, coap_message:ack(Request)),
    coap_exchange:send_message(Pid, Response).

reply_content({_Pid, Sender}, Request=#coap_message{type=non}, Type, Content) ->
    Response = coap_message:content(Type, Content, coap_message:non(Request)),
    coap_endpoint:start_exchange(Sender, Response);

reply_content({Pid, _Sender}, Request=#coap_message{type=con}, Type, Content) ->
    Response = coap_message:content(Type, Content, coap_message:ack(Request)),
    coap_exchange:send_message(Pid, Response).

send_message(Pid, Message) ->
    gen_fsm:send_event(Pid, {out, Message}).

% end of file
