%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% request handler
% provides caching for atomic multi-block operations
% provides synchronous callbacks that block until a response is ready
-module(coap_responder).
-behaviour(gen_server).

-include("coap.hrl").

-export([start_link/2, notify/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state, {channel, prefix, module, args, segs, resource, observer, obseq, timer}).

-define(EXCHANGE_LIFETIME, 247000).

start_link(Channel, Uri) ->
    gen_server:start_link(?MODULE, [Channel, Uri], []).

notify(Uri, Resource) ->
    [gen_server:cast(Pid, Resource) || Pid <- pg2:get_members({coap_observer, Uri})].

init([Channel, Uri]) ->
    % the receiver will be determined based on the URI
    case coap_server_content:get_handler(Uri) of
        {Prefix, Module, Args} ->
            Channel ! {responder_started},
            {ok, #state{channel=Channel, prefix=Prefix, module=Module, args=Args,
                segs=orddict:new(), obseq=0}};
        undefined ->
            {stop, not_found}
    end.

handle_call(_Msg, _From, State) ->
    {reply, unknown_command, State}.

handle_cast(_Resource, State=#state{observer=undefined}) ->
    % ignore unexpected notification
    {noreply, State};
handle_cast(Resource, State=#state{observer=Observer}) ->
    return_resource(Observer, State#state{resource=Resource}).

handle_info({coap_request, ChId, _Channel, undefined, Request}, State) ->
    handle(ChId, Request, State);
handle_info(cache_expired, State=#state{observer=undefined}) ->
    {stop, normal, State};
handle_info(cache_expired, State) ->
    % multi-block cache expired, but the observer is still active
    {noreply, State};
handle_info(Info, State) ->
    io:fwrite("responder unexpected ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #state{channel=Channel}) ->
    Channel ! {responder_completed},
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% handlers

handle(ChId, Request=#coap_message{options=Options}, State=#state{channel=Channel}) ->
    Block1 = proplists:get_value(block1, Options),
    case assemble_payload(Request, Block1, State) of
        {error, Code} ->
            return_response(Request, {error, Code}, State);
        {continue, State2} ->
            coap_channel:send(Channel,
                coap_message:set(block1, Block1,
                    coap_message:response({ok, continue}, Request))),
            set_timeout(State2, ?EXCHANGE_LIFETIME);
        {ok, Payload, State2} ->
            check_resource(ChId, Request#coap_message{payload=Payload}, State2)
    end.

assemble_payload(#coap_message{payload=Payload}, undefined, State) ->
    {ok, Payload, State};
assemble_payload(#coap_message{payload=Segment}, {Num, true, Size}, State=#state{segs=Segs}) ->
    case byte_size(Segment) of
        Size -> {continue, State#state{segs=orddict:store(Num, Segment, Segs)}};
        _Else -> {error, bad_request}
    end;
assemble_payload(#coap_message{payload=Segment}, {_Num, false, _Size}, State=#state{segs=Segs}) ->
    Payload = lists:foldl(
        fun ({Num1, Segment1}, Acc) when Num1*byte_size(Segment1) == byte_size(Acc) ->
                <<Acc/binary, Segment1/binary>>;
            (_Else, _Acc) ->
                throw({error, request_entity_incomplete})
        end, <<>>, orddict:to_list(Segs)),
    {ok, <<Payload/binary, Segment/binary>>, State#state{segs=orddict:new()}}.

check_resource(ChId, Request, State=#state{prefix=Prefix, module=Module, resource=undefined}) ->
    Resource = invoke_callback(Module, coap_get, [ChId, Prefix, uri_suffix(Prefix, Request)]),
    check_preconditions(ChId, Request, State#state{resource=Resource});
check_resource(ChId, Request, State) ->
    check_preconditions(ChId, Request, State).

check_preconditions(ChId, Request, State) ->
    case if_match(Request, State) and if_none_match(Request, State) of
        true ->
            handle_method(ChId, Request, State);
        false ->
            return_response(Request, {error, precondition_failed}, State)
    end.

if_match(#coap_message{options=Options}, #state{resource={ok, #coap_content{etag=ETag}}}) ->
    case proplists:get_value(if_match, Options, []) of
        % empty string matches any existing representation
        [] -> true;
        % match exact resources
        List -> lists:member(ETag, List)
    end;
if_match(#coap_message{options=Options}, #state{resource={error, _}}) ->
    not proplists:is_defined(if_match, Options).

if_none_match(#coap_message{options=Options}, #state{resource={ok, _}}) ->
    not proplists:is_defined(if_none_match, Options);
if_none_match(#coap_message{}, #state{resource={error, _}}) ->
    true.

handle_method(ChId, Request=#coap_message{method='get', options=Options}, State) ->
    case proplists:get_value(observe, Options) of
        0 ->
            handle_observe(ChId, Request, State);
%        [1] ->
%            handle0(Module, [coap_unsubscribe, coap_get], ChId, Channel, Prefix, Suffix, Message);
        undefined ->
            return_resource(Request, State);
        _Else ->
            return_response(Request, {error, bad_option}, State)
    end;
handle_method(ChId, Request=#coap_message{method='post'}, State) ->
%FIXMEEEEEEEEEEE
    handle_put(ChId, Request, State);
handle_method(ChId, Request=#coap_message{method='put'}, State) ->
    handle_put(ChId, Request, State);
handle_method(ChId, Request=#coap_message{method='delete'}, State) ->
    handle_delete(ChId, Request, State);
handle_method(_ChId, Request, State) ->
    return_response(Request, {error, method_not_allowed}, State).

handle_observe(ChId, Request=#coap_message{options=Options},
        State=#state{prefix=Prefix, module=Module, resource={ok, _Content}}) ->
    case invoke_callback(Module, coap_observe, [ChId, Prefix, uri_suffix(Prefix, Request)]) of
        ok ->
            Uri = proplists:get_value(uri_path, Options, []),
            pg2:create({coap_observer, Uri}),
            pg2:join({coap_observer, Uri}, self()),
            return_resource(Request, State#state{observer=Request});
        {error, method_not_allowed} ->
            % observe is not supported, fallback to standard get
            return_resource(Request, State#state{observer=undefined});
        {error, Error} ->
            return_response(Request, {error, Error}, State)
    end;
handle_observe(_ChId, Request, State=#state{resource={error, Code}}) ->
    return_response(Request, {error, Code}, State).

handle_put(ChId, Request, State=#state{prefix=Prefix, module=Module}) ->
    case invoke_callback(Module, coap_put,
            [ChId, Prefix, uri_suffix(Prefix, Request), coap_message:get_content(Request)]) of
        ok ->
            return_response(Request, created_or_changed(State), State);
        {error, Error} ->
            return_response(Request, {error, Error}, State)
    end.

created_or_changed(#state{resource={ok, _}}) ->
    {ok, changed};
created_or_changed(#state{resource={error, not_found}}) ->
    {ok, created}.

handle_delete(ChId, Request, State=#state{prefix=Prefix, module=Module}) ->
    case invoke_callback(Module, coap_delete, [ChId, Prefix, uri_suffix(Prefix, Request)]) of
        ok ->
            return_response(Request, {ok, deleted}, State);
        {error, Error} ->
            return_response(Request, {error, Error}, State)
    end.

invoke_callback(Module, Fun, Args) ->
    case catch Module:module_info(exports) of
        Exports when is_list(Exports) ->
            case lists:member({Fun, length(Args)}, Exports) of
                true ->
                    case catch apply(Module, Fun, Args) of
                        {'EXIT', Error} ->
                            error_logger:error_msg("~p", [Error]),
                            {error, internal_server_error};
                        Response ->
                            Response
                    end;
                false ->
                    {error, method_not_allowed}
            end;
        {'EXIT', {undef, _}} -> {error, service_unavailable}
    end.

return_resource(Request=#coap_message{options=Options},
        State=#state{resource={ok, Content=#coap_content{etag=ETag}}}) ->
    send_observable(Request,
        case lists:member(ETag, proplists:get_value(etag, Options, [])) of
            true ->
                coap_message:set_content(#coap_content{etag=ETag},
                    coap_message:response({ok, valid}, Request));
            false ->
                coap_message:set_content(Content, proplists:get_value(block2, Options),
                    coap_message:response({ok, content}, Request))
        end, State);
return_resource(Request, State=#state{resource={error, Error}}) ->
    return_response(Request, {error, Error}, State).

return_response(Request, Code, State) ->
    return_response(Request, Code, #coap_content{}, State).

return_response(Request, Code, Content, State) ->
    send_response(
        coap_message:set_content(Content,
            coap_message:response(Code, Request)),
        State).

send_observable(#coap_message{token=Token, options=Options}, Response,
        State=#state{observer=Observer, obseq=Seq}) ->
    case {proplists:get_value(observe, Options), Observer} of
        % when requested observe and is observing, return the sequence number
        {0, #coap_message{token=Token}} ->
            send_response(coap_message:set(observe, Seq, Response), State#state{obseq=next_seq(Seq)});
        _Else ->
            send_response(Response, State)
    end.

send_response(Response=#coap_message{options=Options},
        State=#state{channel=Channel, observer=Observer}) ->
    coap_channel:send(Channel, Response),
    case Observer of
        #coap_message{} ->
            % notifications will follow
            {noreply, State};
        undefined ->
            case proplists:get_value(block2, Options) of
                {_, true, _} ->
                    % client is expected to ask for more blocks
                    set_timeout(State, ?EXCHANGE_LIFETIME);
                _Else ->
                    % no further communication concerning this request
                    {stop, normal, State}
            end
    end.

uri_suffix(Prefix, #coap_message{options=Options}) ->
    Uri = proplists:get_value(uri_path, Options, []),
    lists:nthtail(length(Prefix), Uri).

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
