%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
%
-module(coap_dtls_socket).
-behaviour(gen_server).

-export([connect/2, close/1, start_link/1, get_channel/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {sock, supid, channel}).

connect(Host, Port) ->
    {ok, Socket} = gen_server:start_link(?MODULE, [connect, Host, Port], []),
    {ok, Channel} = get_channel(Socket, {Host, Port}),
    {ok, Socket, Channel}.

close(Pid) ->
    gen_server:cast(Pid, shutdown).

start_link(ListenSocket) ->
    gen_server:start_link(?MODULE, [accept, ListenSocket], []).

get_channel(Pid, ChId) ->
    gen_server:call(Pid, {get_channel, ChId}).

init([connect, Host, Port]) ->
    {ok, Sock} = ssl:connect(Host, Port,  [binary, {protocol, dtls}]),
    {ok, #state{sock=Sock}};
init([accept, ListenSocket]) ->
    gen_server:cast(self(), accept),
    {ok, #state{sock=ListenSocket}}.

handle_call({get_channel, ChId}, _From, State=#state{channel=undefined}) ->
    {ok, SupPid, Pid} = coap_channel_sup:start_link(self(), ChId),
    {reply, {ok, Pid}, State#state{supid=SupPid, channel=Pid}}.

handle_cast(accept, State = #state{sock=ListenSocket}) ->
    {ok, Socket} = ssl:transport_accept(ListenSocket),
    % create a new acceptor to maintain a set of waiting acceptors
    coap_dtls_listen:start_socket(),
    % establish the connection
    ok = ssl:ssl_accept(Socket),
    % FIXME: where do we get the chanel id?
    {ok, SupPid, Pid} = coap_channel_sup:start_link(self(), {{0,0,0,0}, 0}),
    {noreply, State#state{sock=Socket, supid=SupPid, channel=Pid}};
handle_cast(shutdown, State) ->
    {stop, normal, State}.

handle_info({ssl, _Socket, Data}, State = #state{channel=Chan}) ->
    Chan ! {datagram, Data},
    {noreply, State};
handle_info({ssl_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({datagram, _ChId, Data}, State=#state{sock=Socket}) ->
    ok = ssl:send(Socket, Data),
    {noreply, State};
handle_info({terminated, SupPid, _ChId}, State=#state{sock=Socket, supid=SupPid}) ->
    % the channel has terminated
    ssl:close(Socket),
    {stop, normal, State};
handle_info(Info, State) ->
    io:fwrite("coap_dtls_socket unexpected ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    %io:fwrite("coap_dtls_socket terminated ~w~n", [Reason]),
    ok.

% end of file
