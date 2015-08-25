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

-export([send/2, reply/3, reply_content/4]).
-export([handle_request/3, handle_response/3, handle_ack/3, handle_error/3]).

-include("coap.hrl").

send(Channel, Method) ->
    coap_channel:send_message(Channel,
        #coap_message{type=con, method=Method}).

reply(Channel, Request, Method) ->
    send_message(Channel,
        coap_message:response(Method, Request)).

reply_content(Channel, Request, Type, Content) ->
    send_message(Channel,
        coap_message:content(Type, Content, coap_message:response(Request))).


% when acting as a server, the receiver is uknown
handle_request(undefined, Channel, Message=#coap_message{}) ->
    % the receiver will be determined based on the URI
    case coap_server_content:get_handler(Message) of
        undefined ->
            reply(Channel, Message, {error, not_found});
        Handler ->
            Handler ! {coap_request, Channel, undefined, Message}
    end.

handle_response({Handler, Ref}, Channel, Message=#coap_message{}) ->
    Handler ! {coap_response, Channel, Ref, Message}.

handle_ack({Handler, Ref}, Channel, _Message) ->
    Handler ! {coap_ack, Channel, Ref}.

handle_error({Handler, Ref}, Channel, {MsgId, Error}) ->
    Handler ! {coap_error, Channel, Ref, Error}.


send_message(Channel, Message) ->
    case Message of
        #coap_message{type=ack} -> coap_channel:send_ack(Channel, Message);
        #coap_message{} -> coap_channel:send_message(Channel, Message)
    end.

% end of file
