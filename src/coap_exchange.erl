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

-export([ack/3, reply/4, reply/5, reply_content/5, send_message/2]).

ack(_Pid, Sender, Request=#coap_message{type=non}) ->
    ok;

ack(Pid, _Sender, Request=#coap_message{type=con}) ->
    Response = coap_message:response(Request),
    coap_exchange:send_message(Pid, Response).

reply(Pid, Sender, Request, Method) ->
    reply(Pid, Sender, Request, Method, <<>>).

reply(_Pid, Sender, Request=#coap_message{type=non}, Method, Payload) ->
    Response = coap_message:response(Method, Payload, Request),
    coap_endpoint:start_exchange(Sender, undefined, Response);

reply(Pid, _Sender, Request=#coap_message{type=con}, Method, Payload) ->
    Response = coap_message:response(Method, Payload, Request),
    coap_exchange:send_message(Pid, Response).

reply_content(_Pid, Sender, Request=#coap_message{type=non}, Type, Content) ->
    Response = coap_message:response_with_content(Type, Content, Request),
    coap_endpoint:start_exchange(Sender, undefined, Response);

reply_content(Pid, _Sender, Request=#coap_message{type=con}, Type, Content) ->
    Response = coap_message:response_with_content(Type, Content, Request),
    coap_exchange:send_message(Pid, Response).

send_message(Pid, Message) ->
    gen_fsm:send_event(Pid, {out, Message}).

% end of file
