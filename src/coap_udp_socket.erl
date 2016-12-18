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

-export([start_link/0, start_link/2, get_channel/2, close/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {sock, chans, pool}).

% client
start_link() ->
    gen_server:start_link(?MODULE, [0], []).
% server
start_link(InPort, SupPid) ->
    gen_server:start_link(?MODULE, [InPort, SupPid], []).

get_channel(Pid, {PeerIP, PeerPortNo}) ->
    gen_server:call(Pid, {get_channel, {PeerIP, PeerPortNo}}).

close(Pid) ->
    % the channels will be terminated by their supervisor (server), or
    % should be terminated by the user (client)
    gen_server:cast(Pid, shutdown).


init([InPort]) ->
    {ok, Socket} = gen_udp:open(InPort, [binary, {active, true}, {reuseaddr, true}]),
    %{ok, InPort2} = inet:port(Socket),
    %error_logger:info_msg("coap listen on *:~p~n", [InPort2]),
    {ok, #state{sock=Socket, chans=dict:new()}};

init([InPort, SupPid]) ->
    gen_server:cast(self(), {set_pool, SupPid}),
    init([InPort]).

handle_call({get_channel, ChId}, _From, State=#state{chans=Chans, pool=undefined}) ->
    case find_channel(ChId, Chans) of
        {ok, Pid} ->
            {reply, {ok, Pid}, State};
        undefined ->
            {ok, _, Pid} = coap_channel_sup:start_link(self(), ChId),
            {reply, {ok, Pid}, store_channel(ChId, Pid, State)}
    end;
handle_call({get_channel, ChId}, _From, State=#state{pool=PoolPid}) ->
    case coap_channel_sup_sup:start_channel(PoolPid, ChId) of
        {ok, _, Pid} ->
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
handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    io:fwrite("coap_udp_socket unknown cast ~p~n", [Request]),
    {noreply, State}.

handle_info({udp, _Socket, PeerIP, PeerPortNo, Data}, State=#state{chans=Chans, pool=PoolPid}) ->
    ChId = {PeerIP, PeerPortNo},
    case find_channel(ChId, Chans) of
        % channel found in cache
        {ok, Pid} ->
            Pid ! {datagram, Data},
            {noreply, State};
        undefined when is_pid(PoolPid) ->
            case coap_channel_sup_sup:start_channel(PoolPid, ChId) of
                % new channel created
                {ok, _, Pid} ->
                    Pid ! {datagram, Data},
                    {noreply, store_channel(ChId, Pid, State)};
                % drop this packet
                {error, _} ->
                    {noreply, State}
            end;
        undefined ->
            % ignore unexpected message received by a client
            % TODO: do we want to send reset?
            {noreply, State}
    end;
handle_info({datagram, {PeerIP, PeerPortNo}, Data}, State=#state{sock=Socket}) ->
    ok = gen_udp:send(Socket, PeerIP, PeerPortNo, Data),
    {noreply, State};
handle_info({terminated, SupPid, ChId}, State=#state{chans=Chans}) ->
    Chans2 = dict:erase(ChId, Chans),
    exit(SupPid, normal),
    {noreply, State#state{chans=Chans2}};
handle_info(Info, State) ->
    io:fwrite("coap_udp_socket unexpected ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{sock=Sock}) ->
    gen_udp:close(Sock),
    ok.


find_channel(ChId, Chans) ->
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

store_channel(ChId, Pid, State=#state{chans=Chans}) ->
    State#state{chans=dict:store(ChId, Pid, Chans)}.

% end of file
