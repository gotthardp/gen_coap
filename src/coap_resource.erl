%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(coap_resource).

-include("coap.hrl").

% called when a client asks for .well-known/core resources
-callback coap_discover([binary()], any()) ->
    [coap_uri()].

% GET handler
-callback coap_get(coap_channel_id(), [binary()], [binary()], [binary()], coap_content()) ->
    coap_content() | {'error', atom()}.
% POST handler
-callback coap_post(coap_channel_id(), [binary()], [binary()], coap_content()) ->
    {'ok', atom(), coap_content()} | {'error', atom()}.
% PUT handler
-callback coap_put(coap_channel_id(), [binary()], [binary()], coap_content()) ->
    'ok' | {'error', atom()}.
% DELETE handler
-callback coap_delete(coap_channel_id(), [binary()], [binary()]) ->
    'ok' | {'error', atom()}.

% observe request handler
-callback coap_observe(coap_channel_id(), [binary()], [binary()], boolean(), coap_content()) ->
    {'ok', any()} | {'error', atom()}.
% cancellation request handler
-callback coap_unobserve(any()) ->
    'ok'.
% handler for messages sent to the responder process
% used to generate notifications
-callback handle_info(any(), any()) ->
    {'notify', any(), coap_content(), any()} | {'noreply', any()} | {'stop', any()}.
% response to notifications
-callback coap_ack(any(), any()) ->
    {'ok', any()}.

-type coap_channel_id() :: {inet:port_number(), inet:ip_address()}.
-type coap_uri() :: {'absolute', [binary()], coap_uri_param()}.
-type coap_uri_param() :: {atom(), binary()}.

% end of file
