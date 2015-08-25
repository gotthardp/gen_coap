%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% dispatcher for UDP communication
% maintains a lookup-table for existing channels
% when a channel pool is provided (server mode), creates new channels
-module(coap_udp_socket).
-behaviour(gen_server).

-export([start_link/0, start_link/2, connect/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {sock, chans, pool}).

% client
start_link() ->
    gen_server:start_link(?MODULE, [0], []).
% server
start_link(InPort, SupPid) ->
    gen_server:start_link(?MODULE, [InPort, SupPid], []).

connect(Pid, {PeerIP, PeerPortNo}) ->
    gen_server:call(Pid, {connect, {PeerIP, PeerPortNo}}).

init([InPort]) ->
    {ok, Socket} = gen_udp:open(InPort, [binary, {active, true}]),
    {ok, InPort2} = inet:port(Socket),
    error_logger:info_msg("coap listen on *:~p~n", [InPort2]),
    {ok, #state{sock=Socket, chans=dict:new()}};

init([InPort, SupPid]) ->
    gen_server:cast(self(), {set_pool, SupPid}),
    init([InPort]).

handle_call({connect, ChId}, _From, State=#state{chans=Chans, pool=undefined}) ->
    case get_channel(ChId, Chans) of
        {ok, Pid} ->
            {reply, {ok, Pid}, State};
        undefined ->
            {ok, Pid} = coap_channel:start_link(self(), ChId),
            {reply, {ok, Pid}, store_channel(ChId, Pid, State)}
    end;
handle_call({connect, ChId}, _From, State=#state{pool=PoolPid}) ->
    case start_channel(PoolPid, ChId) of
        {ok, Pid} ->
            {reply, {ok, Pid}, store_channel(ChId, Pid, State)};
        Error ->
            {reply, Error, State}
    end;
handle_call(_Unknown, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({set_pool, SupPid}, State) ->
    % calling coap_server directly from init/1 causes deadlock
    PoolPid = coap_server:channel_sup(SupPid),
    {noreply, State#state{pool=PoolPid}};
handle_cast(Request, State) ->
    io:fwrite("coap_udp_socket unknown cast ~p~n", [Request]),
    {noreply, State}.

handle_info({udp, _Socket, PeerIP, PeerPortNo, Data}, State=#state{chans=Chans, pool=PoolPid}) ->
    ChId = {PeerIP, PeerPortNo},
    case get_channel(ChId, Chans) of
        % channel found in cache
        {ok, Pid} ->
            Pid ! {datagram, Data},
            {noreply, State};
        undefined when is_pid(PoolPid) ->
            case start_channel(PoolPid, ChId) of
                % new channel created
                {ok, Pid} ->
                    Pid ! {datagram, Data},
                    {noreply, store_channel(ChId, Pid, State)};
                % channel processor crashed and will be restarted soon
                % drop this packet and wait for retransmission
                {error, already_present} ->
                    {noreply, State}
            end;
        undefined ->
            % ignore unexpected message received by a client
            % TODO: do we want to send reset?
            {noreply, State}
    end;
handle_info({datagram, {PeerIP, PeerPortNo}, Data}, State=#state{sock=Socket}) ->
    gen_udp:send(Socket, PeerIP, PeerPortNo, Data),
    {noreply, State};
handle_info({terminated, ChId}, State=#state{chans=Chans}) ->
    Chans2 = dict:erase(ChId, Chans),
    {noreply, State#state{chans=Chans2}};
handle_info(Info, State) ->
    io:fwrite("coap_udp_socket unexpected massage ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    io:fwrite("endpoint terminated ~p~n", [Reason]),
    ok.


get_channel(ChId, Chans) ->
    case dict:find(ChId, Chans) of
        % there is a channel in our cache, but it might have crashed
        {ok, Pid} ->
            case erlang:is_process_alive(Pid) of
                true -> {ok, Pid};
                false -> undefined
            end;
        % we got data via a new channel
        error -> undefined
    end.

start_channel(PoolPid, ChId) ->
    case coap_channel_sup:start_channel(PoolPid, ChId) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        {error, OtherError} -> {error, OtherError}
    end.

store_channel(ChId, Pid, State=#state{chans=Chans}) ->
    State#state{chans=dict:store(ChId, Pid, Chans)}.

% end of file
