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
        coap_message:response(Method, Request)).

reply(Channel, Request, Method, Payload) ->
    send_message(Channel,
        coap_message:response(Method, Payload, Request)).

reply_content(Channel, Request, Format, Content) ->
    send_message(Channel,
        coap_message:set(etag, binary:part(crypto:hash(sha, Content), {0,4}),
            coap_message:set(content_format, Format,
                coap_message:set_payload(Content,
                    coap_message:response({ok, content}, Request))))).


% when acting as a server, the receiver is uknown
handle_request(undefined, ChId, Channel, Message=#coap_message{options=Options}) ->
    % the receiver will be determined based on the URI
    Uri = proplists:get_value(uri_path, Options, []),
    case coap_server_content:get_handler(Uri) of
        [] ->
            reply(Channel, Message, {error, not_found});
        [{Prefix, Module, Args}] ->
            Suffix = lists:nthtail(length(Prefix), Uri),
            call_module(Module, ChId, Channel, Suffix, Message)
    end.

handle_response({Sender, Ref}, ChId, Channel, Message=#coap_message{}) ->
    Sender ! {coap_response, ChId, Channel, Ref, Message}.

handle_ack({Sender, Ref}, ChId, Channel, _Message) ->
    Sender ! {coap_ack, ChId, Channel, Ref}.

handle_error({Sender, Ref}, ChId, Channel, {MsgId, Error}) ->
    Sender ! {coap_error, ChId, Channel, Ref, Error}.


call_module(Module, ChId, Channel, Suffix, Message=#coap_message{method='get', options=Options}) ->
    case proplists:get_value(observe, Options) of
        [0] ->
            call_module0(Module, [coap_subscribe, coap_get], ChId, Channel, Suffix, Message);
        [1] ->
            call_module0(Module, [coap_unsubscribe, coap_get], ChId, Channel, Suffix, Message);
        undefined ->
            call_module0(Module, [coap_get], ChId, Channel, Suffix, Message);
        _SomethingWeird ->
            reply(Channel, Message, {error, bad_option})
    end;
call_module(Module, ChId, Channel, Suffix, Message=#coap_message{method='post'}) ->
    call_module0(Module, [coap_post], ChId, Channel, Suffix, Message);
call_module(Module, ChId, Channel, Suffix, Message=#coap_message{method='put'}) ->
    call_module0(Module, [coap_put], ChId, Channel, Suffix, Message);
call_module(Module, ChId, Channel, Suffix, Message=#coap_message{method='delete'}) ->
    call_module0(Module, [coap_delete], ChId, Channel, Suffix, Message).

call_module0(Module, [Method|MoreMethods], ChId, Channel, Suffix, Message) ->
    case erlang:function_exported(Module, Method, 4) of
        true -> apply(Module, Method, [ChId, Channel, Suffix, Message]);
        false -> call_module0(Module, MoreMethods, ChId, Channel, Suffix, Message)
    end;
call_module0(_Module, [], _ChId, Channel, _Suffix, Message) ->
    reply(Channel, Message, {error, method_not_allowed}).


send_message(Channel, Message) ->
    case Message of
        #coap_message{type=ack} -> coap_channel:send_ack(Channel, Message);
        #coap_message{} -> coap_channel:send_message(Channel, Message)
    end.

% end of file
