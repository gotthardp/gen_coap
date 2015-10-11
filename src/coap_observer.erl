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

-record(state, {client, sock, channel, ref, lastseq}).

observe(Uri) ->
    observe(Uri, []).
observe(Uri, Options) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [self(), Uri, Options], []),
    % receive the initial response
    receive
        {coap_notify, {ok, Code}, Content} ->
            {ok, Code, Content};
        {coap_notify, {error, Code}} ->
            {error, Code}
    end.

stop(Pid) ->
    gen_server:cast(Pid, shutdown).


init([Client, Uri, Options]) ->
    {PeerIP, PortNo, Path} = coap_client:resolve_uri(Uri),
    {ok, Sock} = coap_udp_socket:start_link(),
    {ok, Channel} = coap_udp_socket:get_channel(Sock, {PeerIP, PortNo}),
    % observe the resource
    {ok, Ref} = coap_channel:send(Channel,
        coap_message:request(con, get, <<>>, [{uri_path, Path}, {observe, 0} | Options])),
    {ok, #state{client=Client, sock=Sock, channel=Channel, ref=Ref}}.

handle_call(_Unknown, _From, State) ->
    {reply, unknown_call, State}.

handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    io:fwrite("coap_observer unknown cast ~p~n", [Request]),
    {noreply, State}.

handle_info({coap_response, _ChId, Channel, Ref, Message=#coap_message{method={ok, Code}, options=Options, payload=Data}},
        State=#state{client=Client, channel=Channel, ref=Ref}) ->
    Client ! {coap_notify, {ok, Code}, coap_message:get_content(Message)},
    {noreply, State};
handle_info({coap_response, _ChId, Channel, Ref, Message=#coap_message{method={error, Code}}},
        State=#state{client=Client, channel=Channel, ref=Ref}) ->
    Client ! {coap_notify, {error, Code}},
    {stop, normal, State};
handle_info({coap_error, _ChId, Channel, Ref, reset},
        State=#state{client=Client, channel=Channel, ref=Ref}) ->
    {stop, normal, State};
handle_info(Info, State) ->
    io:fwrite("coap_observer unexpected ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{sock=Sock, channel=Channel}) ->
    coap_channel:close(Channel),
    coap_udp_socket:close(Sock),
    ok.

% end of file
