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

-record(state, {channel, prefix, module, args, cached, timer}).

-define(MAX_BLOCK_SIZE, 1024).
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
            {ok, #state{channel=Channel, prefix=Prefix, module=Module, args=Args}};
        undefined ->
            {stop, not_found}
    end.

handle_call(_Msg, _From, State) ->
    {reply, unknown_command, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({coap_request, ChId, Channel, undefined, Message}, State) ->
    handle(fun check_method/4, ChId, Channel, Message, State);
handle_info(cache_expired, State) ->
    {stop, normal, State};
handle_info(Info, State) ->
    io:fwrite("responder unexpected ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, State=#state{channel=Channel}) ->
    Channel ! {responder_completed},
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% utility functions

handle(Fun, ChId, Channel, Request, State) ->
    case Fun(ChId, Channel, Request, State) of
        {continue, Fun2, State2} ->
            handle(Fun2, ChId, Channel, Request, State2);
        {stop, Code} ->
            coap_channel:send(Channel,
                coap_message:response(Code, Request)),
            {stop, normal, State};
        {respond, Response, State2} ->
            coap_channel:send(Channel, Response),
            case is_blockwise(Request) or is_blockwise(Response) of
                true -> set_timeout(State2, ?EXCHANGE_LIFETIME);
                false -> {stop, normal, State2}
            end
    end.

is_blockwise(Message=#coap_message{options=Options}) ->
    proplists:is_defined(block1, Options) or proplists:is_defined(block2, Options).

set_timeout(State=#state{timer=undefined}, Timeout) ->
    set_timeout0(State, Timeout);
set_timeout(State=#state{timer=Timer}, Timeout) ->
    erlang:cancel_timer(Timer),
    set_timeout0(State, Timeout).

set_timeout0(State, Timeout) ->
    Timer = erlang:send_after(Timeout, self(), cache_expired),
    {noreply, State#state{timer=Timer}}.

check_method(_ChId, _Channel, Request, State) ->
    case Request of
        #coap_message{method='get'} -> {continue, fun check_observe/4, State};
        #coap_message{method='post'} -> {continue, fun check_post_callbacks/4, State};
        #coap_message{method='put'} -> {continue, fun check_put_callbacks/4, State};
        #coap_message{method='delete'} -> {continue, fun check_delete_callbacks/4, State};
        _Else -> {stop, {error, method_not_allowed}}
    end.

check_observe(ChId, Channel, Request=#coap_message{options=Options}, State) ->
    case proplists:get_value(observe, Options) of
%        [0] ->
%            handle_subscribe(Module, ChId, Channel, Prefix, Suffix, Message);
%        [1] ->
%            handle0(Module, [coap_unsubscribe, coap_get], ChId, Channel, Prefix, Suffix, Message);
        undefined -> {continue, fun check_get_callbacks/4, State};
        _Else -> {stop, {error, bad_option}}
    end.

check_get_callbacks(_ChId, _Channel, _Request, State=#state{module=Module}) ->
    case erlang:function_exported(Module, coap_get, 4) of
        true -> {continue, fun check_valid/4, State};
        false -> {stop, {error, method_not_allowed}}
    end.

check_valid(_ChId, _Channel, #coap_message{options=Options},
        State=#state{cached=#coap_resource{etag=ETag}}) ->
    case lists:member(ETag, proplists:get_value(etag, Options, [])) of
        true -> {stop, {ok, valid}};
        false -> {continue, fun get_resource/4, State}
    end;
check_valid(_ChId, _Channel, _Message, State) ->
    {continue, fun get_resource/4, State}.

get_resource(ChId, _Channel, Request=#coap_message{options=Options},
        State=#state{prefix=Prefix, module=Module}) ->
    case apply(Module, coap_get, [ChId, Prefix, uri_suffix(Prefix, Request), Request]) of
        {ok, Resource=#coap_resource{}} ->
            Block2 = proplists:get_value(block2, Options),
            {respond, coap_message:set_resource(Resource, Block2,
                          coap_message:response({ok, content}, Request)),
                      State#state{cached=Resource}};
        {error, Error} ->
            {stop, {error, Error}}
    end.

check_post_callbacks(_ChId, _Channel, _Request, State=#state{module=Module}) ->
    case erlang:function_exported(Module, coap_post, 4) of
        true -> {continue, fun check_valid/4, State};
        false -> {stop, {error, method_not_allowed}}
    end.

check_put_callbacks(_ChId, _Channel, _Request, State=#state{module=Module}) ->
    case erlang:function_exported(Module, coap_put, 4) of
        true -> {continue, fun check_valid/4, State};
        false -> {stop, {error, method_not_allowed}}
    end.

check_delete_callbacks(_ChId, _Channel, _Request, State=#state{module=Module}) ->
    case erlang:function_exported(Module, coap_delete, 4) of
        true -> {continue, fun check_valid/4, State};
        false -> {stop, {error, method_not_allowed}}
    end.

uri_suffix(Prefix, #coap_message{options=Options}) ->
    Uri = proplists:get_value(uri_path, Options, []),
    lists:nthtail(length(Prefix), Uri).

next_seq(Seq) ->
    if
        Seq < 16#0FFF -> Seq+1;
        true -> 0
    end.

% end of file
