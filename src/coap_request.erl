%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% functions for request creation and processing
-module(coap_request).

-export([send/3, send/4, send/5, ack/2, reply/3, reply/4, reply_content/4]).
-export([handle_request/4, handle_response/4, handle_ack/4, handle_error/4]).

-include("coap.hrl").

send(Channel, Type, Method) ->
    send(Channel, Type, Method, <<>>, []).

send(Channel, Type, Method, Payload) ->
    send(Channel, Type, Method, Payload, []).

send(Channel, Type, Method, Payload, Options) ->
    coap_channel:send_message(Channel,
        #coap_message{type=Type, method=Method, payload=Payload, options=Options}).


ack(Channel, Request) ->
    send_message(Channel,
        coap_message:response(Request)).

reply(Channel, Request, Method) ->
    send_message(Channel,
        coap_message:response(Request, Method)).

reply(Channel, Request, Method, Payload) ->
    send_message(Channel,
        coap_message:response(Request, Method, Payload)).

reply_content(Channel, Request, Format, Content) ->
    send_message(Channel,
        coap_message:content(Format, Content, coap_message:response(Request))).


% when acting as a server, the receiver is uknown
handle_request(undefined, ChId, Channel, Message=#coap_message{}) ->
    % the receiver will be determined based on the URI
    case coap_server_content:get_handler(Message) of
        undefined ->
            reply(Channel, Message, {error, not_found});
        Handler ->
            Handler ! {coap_request, ChId, Channel, undefined, Message}
    end.

handle_response({Handler, Ref}, ChId, Channel, Message=#coap_message{}) ->
    Handler ! {coap_response, ChId, Channel, Ref, Message}.

handle_ack({Handler, Ref}, ChId, Channel, _Message) ->
    Handler ! {coap_ack, ChId, Channel, Ref}.

handle_error({Handler, Ref}, ChId, Channel, {MsgId, Error}) ->
    Handler ! {coap_error, ChId, Channel, Ref, Error}.


send_message(Channel, Message) ->
    case Message of
        #coap_message{type=ack} -> coap_channel:send_ack(Channel, Message);
        #coap_message{} -> coap_channel:send_message(Channel, Message)
    end.

% end of file
