%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(coap_observer).
-behaviour(gen_server).

-export([observe/1, observe/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("coap.hrl").

-record(state, {client, scheme, sock, channel, ropt, ref, lastseq}).

observe(Uri) ->
    observe(Uri, []).
observe(Uri, Options) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [self(), Uri, Options], []),
    % receive the initial response
    receive
        % request successful, but not subscribed
        {coap_notify, Pid, undefined, {ok, Code}, Content} ->
            {ok, Code, Content};
        % subscribe successful
        {coap_notify, Pid, N, {ok, Code}, Content} ->
            {ok, Pid, N, Code, Content};
        % error
        {coap_notify, Pid, undefined, {error, Code}, #coap_content{payload= <<>>}} ->
            {error, Code};
        {coap_notify, Pid, undefined, {error, Code}, Content} ->
            {error, Code, Content}
    end.

stop(Pid) ->
    gen_server:call(Pid, shutdown).


init([Client, Uri, Options]) ->
    {Scheme, ChId, Path, Query} = coap_client:resolve_uri(Uri),
    {ok, Sock, Channel} = case Scheme of
        coap ->
            {ok, So} = coap_udp_socket:start_link(),
            {ok, Ch} = coap_udp_socket:get_channel(So, ChId),
            {ok, So, Ch};
        coaps ->
            {Host, Port} = ChId,
            coap_dtls_socket:connect(Host, Port)
    end,
    % observe the resource
    ROpt = [{uri_path, Path}, {uri_query, Query}|Options],
    {ok, Ref} = coap_channel:send(Channel,
        coap_message:request(con, get, <<>>, [{observe, 0}|ROpt])),
    {ok, #state{client=Client, scheme=Scheme, sock=Sock, channel=Channel, ropt=ROpt, ref=Ref}}.

handle_call(shutdown, _From, State=#state{channel=Channel, ropt=ROpt}) ->
    {ok, Ref} = coap_channel:send(Channel,
        coap_message:request(con, get, <<>>, [{observe, 1}|ROpt])),
    Reply = coap_client:await_response(Channel, get, ROpt, Ref, #coap_content{}),
    {stop, normal, Reply, State};
handle_call(_Unknown, _From, State) ->
    {reply, unknown_call, State}.

handle_cast(Request, State) ->
    io:fwrite("coap_observer unknown cast ~p~n", [Request]),
    {noreply, State}.

handle_info({coap_response, _ChId, Channel, Ref, Message=#coap_message{type=con}},
        State=#state{channel=Channel, ref=Ref}) ->
    % response or notification arrived as a separate confirmable message
    {ok, _} = coap_client:ack(Channel, Message),
    handle_response(Message, State);
handle_info({coap_response, _ChId, Channel, Ref, Message},
        State=#state{channel=Channel, ref=Ref}) ->
    handle_response(Message, State);
handle_info({coap_error, _ChId, Channel, Ref, reset},
        State=#state{client=Client, channel=Channel, ref=Ref}) ->
    Client ! {coap_notify, self(), undefined, {error, reset}, #coap_content{}},
    {stop, normal, State};
handle_info(Info, State) ->
    io:fwrite("coap_observer unexpected ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{scheme=coap, sock=Sock, channel=Channel}) ->
    coap_channel:close(Channel),
    coap_udp_socket:close(Sock),
    ok;
terminate(_Reason, #state{scheme=coaps, sock=Sock, channel=Channel}) ->
    coap_channel:close(Channel),
    coap_dtls_socket:close(Sock),
    ok.


handle_response(Message=#coap_message{method={ok, _Code}, options=Options, payload=Payload}, State) ->
    case proplists:get_value(block2, Options) of
        {0, true, _Size} = Block2 ->
            request_more_blocks(Message, Block2, Payload, State);
        _Else ->
            send_notify(Message, State)
    end;
handle_response(Message=#coap_message{method={error, Code}}, State=#state{client=Client}) ->
    Client ! {coap_notify, self(), undefined, {error, Code}, coap_message:get_content(Message)},
    {stop, normal, State}.

request_more_blocks(Notify, {Num, true, Size}, Fragment,
        State=#state{client=Client, channel=Channel, ropt=ROpt}) ->
    {ok, Ref2} = coap_channel:send(Channel,
        coap_message:request(con, get, <<>>, [{block2, {Num+1, false, Size}}|ROpt])),
    receive
        {coap_response, _ChId, Channel, Ref2, #coap_message{method={ok, _Code}, options=Options, payload=Data}} ->
            case proplists:get_value(block2, Options) of
                {_Num2, true, _Size2} = Block2 ->
                    request_more_blocks(Notify, Block2, <<Fragment/binary, Data/binary>>, State);
                _Else ->
                    send_notify(Notify#coap_message{payload= <<Fragment/binary, Data/binary>>}, State)
            end;
        {coap_response, _ChId, Channel, Ref2, Message=#coap_message{method={error, Code}}} ->
            Client ! {coap_notify, self(), undefined, {error, Code}, coap_message:get_content(Message)},
            {stop, normal, State}
    end.

send_notify(Message=#coap_message{method={ok, Code}, options=Options},
        State=#state{client=Client, lastseq=LastSeq}) ->
    case proplists:get_value(observe, Options) of
        undefined ->
            % subscription is terminated
            Client ! {coap_notify, self(), undefined, {ok, Code}, coap_message:get_content(Message)},
            {stop, normal, State};
        N when LastSeq == undefined; N > LastSeq ->
            % report and stay subscribed
            Client ! {coap_notify, self(), N, {ok, Code}, coap_message:get_content(Message)},
            {noreply, State#state{lastseq=N}};
        N when N =< LastSeq ->
            % ignore, but stay subscribed
            {noreply, State}
    end.

% end of file
