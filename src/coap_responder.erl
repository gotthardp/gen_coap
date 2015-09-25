%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% provides caching for atomic multi-block operations
-module(coap_responder).
-behaviour(gen_server).

-include("coap.hrl").

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state, {channel, prefix, module, args, segs, cached, timer}).

-define(EXCHANGE_LIFETIME, 247000).

start_link(Channel, Uri) ->
    gen_server:start_link(?MODULE, [Channel, Uri], []).

notify(Resource, Request) ->
    [gen_server:cast(Pid, Request) || Pid <- pg2:get_members(Resource)].

init([Channel, Uri]) ->
    % the receiver will be determined based on the URI
    case coap_server_content:get_handler(Uri) of
        {Prefix, Module, Args} ->
            Channel ! {responder_started},
            {ok, #state{channel=Channel, prefix=Prefix, module=Module, args=Args, segs=orddict:new()}};
        undefined ->
            {stop, not_found}
    end.

handle_call(_Msg, _From, State) ->
    {reply, unknown_command, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({coap_request, ChId, Channel, undefined, Request}, State) ->
    case catch handle(ChId, Channel, Request, State) of
        {cache, Expires, State2} ->
            set_timeout(State2, Expires);
        {stop, State2} ->
            {stop, normal, State2};
        {Code, Error} ->
            coap_channel:send(Channel,
                coap_message:response({Code, Error}, Request)),
            {stop, normal, State}
    end;
handle_info(cache_expired, State) ->
    {stop, normal, State};
handle_info(Info, State) ->
    io:fwrite("responder unexpected ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #state{channel=Channel}) ->
    Channel ! {responder_completed},
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% handlers

handle(ChId, Channel, Request=#coap_message{options=Options}, State) ->
    Block1 = proplists:get_value(block1, Options),
    case assemble_payload(Request, Block1, State) of
        {continue, State2} ->
            coap_channel:send(Channel,
                coap_message:set(block1, Block1,
                    coap_message:response({ok, continue}, Request))),
            {cache, ?EXCHANGE_LIFETIME, State2};
        {ok, Payload, State2} ->
            handle_method(ChId, Channel, Request, State)
    end.

assemble_payload(Request=#coap_message{payload=Payload}, undefined, State) ->
    {ok, Payload, State};
assemble_payload(Request=#coap_message{payload=Segment}, {Num, true, Size}, State=#state{segs=Segs}) ->
    case byte_size(Segment) of
        Size -> {continue, State#state{segs=orddict:store(Num, Segment, Segs)}};
        _Else -> throw({error, bad_request})
    end;
assemble_payload(Request=#coap_message{payload=Segment}, {Num, false, Size}, State=#state{segs=Segs}) ->
    Payload = lists:foldl(
        fun ({Num, Segment}, Acc) when Num*byte_size(Segment) == byte_size(Acc) ->
                <<Acc/binary, Segment/binary>>;
            (_Else, _Acc) ->
                throw({error, request_entity_incomplete})
        end, <<>>, orddict:to_list(Segs)),
    {ok, <<Payload/binary, Segment/binary>>, State#state{segs=orddict:new()}}.

handle_method(ChId, Channel, Request=#coap_message{method='get', options=Options}, State) ->
    case proplists:get_value(observe, Options) of
%        [0] ->
%            handle_subscribe(Module, ChId, Channel, Prefix, Suffix, Message);
%        [1] ->
%            handle0(Module, [coap_unsubscribe, coap_get], ChId, Channel, Prefix, Suffix, Message);
        undefined ->
            ok = cache_invalid(Request, State),
            get_resource(ChId, Channel, Request, State);
        _Else -> {stop, {error, bad_option}}
    end;
handle_method(ChId, Channel, Request=#coap_message{method='post', options=Options}, State) ->
    throw({error, method_not_allowed});
handle_method(ChId, Channel, Request=#coap_message{method='put', options=Options}, State) ->
    throw({error, method_not_allowed});
handle_method(ChId, Channel, Request=#coap_message{method='delete', options=Options}, State) ->
    throw({error, method_not_allowed});
handle_method(_ChId, _Channel, _Request, _State) ->
    throw({error, method_not_allowed}).

cache_invalid(#coap_message{options=Options}, #state{cached=#coap_resource{etag=ETag}}) ->
    case lists:member(ETag, proplists:get_value(etag, Options, [])) of
        true -> throw({ok, valid});
        false -> ok
    end;
cache_invalid(_Message, _State) ->
    ok.

get_resource(ChId, Channel, Request, State=#state{prefix=Prefix, module=Module}) ->
    case invoke_callback(Module, coap_get, [ChId, Prefix, uri_suffix(Prefix, Request), Request]) of
        {ok, Resource=#coap_resource{}} ->
            send_resource(Channel, Request, Resource, State#state{cached=Resource});
        {error, Error} ->
            throw({error, Error})
    end.

send_resource(Channel, Request=#coap_message{options=Options}, Resource, State) ->
    Block2 = proplists:get_value(block2, Options),
    Response = coap_message:set_resource(Resource, Block2,
                   coap_message:response({ok, content}, Request)),
    coap_channel:send(Channel, Response),
    case is_blockwise(Request) or is_blockwise(Response) of
        true -> {cache, ?EXCHANGE_LIFETIME, State};
        false -> {stop, State}
    end.

invoke_callback(Module, Fun, Args) ->
    case catch Module:module_info(exports) of
        Exports when is_list(Exports) ->
            case lists:member({Fun, length(Args)}, Exports) of
                true -> apply(Module, Fun, Args);
                false -> throw({error, method_not_allowed})
            end;
        {'EXIT', {undef, _}} -> throw({error, service_unavailable})
    end.

uri_suffix(Prefix, #coap_message{options=Options}) ->
    Uri = proplists:get_value(uri_path, Options, []),
    lists:nthtail(length(Prefix), Uri).

is_blockwise(#coap_message{options=Options}) ->
    proplists:is_defined(block1, Options) or proplists:is_defined(block2, Options).

next_seq(Seq) ->
    if
        Seq < 16#0FFF -> Seq+1;
        true -> 0
    end.

set_timeout(State=#state{timer=undefined}, Timeout) ->
    set_timeout0(State, Timeout);
set_timeout(State=#state{timer=Timer}, Timeout) ->
    erlang:cancel_timer(Timer),
    set_timeout0(State, Timeout).

set_timeout0(State, Timeout) ->
    Timer = erlang:send_after(Timeout, self(), cache_expired),
    {noreply, State#state{timer=Timer}}.

% end of file
