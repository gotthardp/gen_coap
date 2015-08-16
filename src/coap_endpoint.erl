%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% network client/server, supports UDP for now
-module(coap_endpoint).
-behaviour(gen_server).

-include("coap.hrl").

-export([start_link/3, start_link/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([start_exchange/3, send_message/2]).

-define(MAX_MESSAGE_ID, 65535). % 16-bit number

-record(state, {socket, handler, nextid, in_exch, out_exch}).

% dynamic endpoint
start_link(Handler, InExPid, OutExPid) ->
    gen_server:start_link(?MODULE, [0, Handler, InExPid, OutExPid], []).

% registered endpoint
start_link(ServerName, InPort, Handler, InExPid, OutExPid) ->
    gen_server:start_link(ServerName, ?MODULE, [InPort, Handler, InExPid, OutExPid], []).

% called by handlers to initiate new exchanges
start_exchange({EndPid, PeerIP, PeerPortNo}, UserData, Message) ->
    gen_server:cast(EndPid, {start_exchange, PeerIP, PeerPortNo, Message, self(), UserData}).

% called by exchanges to send binary messages
send_message({EndPid, PeerIP, PeerPortNo}, BinMessage) when is_binary(BinMessage) ->
    gen_server:cast(EndPid, {send_message, PeerIP, PeerPortNo, BinMessage}).

init([InPort, Handler, InPid, OutPid]) ->
    {ok, Socket} = gen_udp:open(InPort, [binary, {active, true}]),
    random:seed(os:timestamp()),
    {ok, #state{socket=Socket, handler=Handler, nextid=random:uniform(?MAX_MESSAGE_ID), in_exch=InPid, out_exch=OutPid}}.

inc_id(MsgId) ->
    if
        MsgId < ?MAX_MESSAGE_ID -> MsgId + 1;
        true -> 1 % or 0?
    end.

handle_call(_Unknown, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({start_exchange, PeerIP, PeerPortNo, Message, Handler, UserData},
            State=#state{nextid=MsgId, out_exch=OutPid}) ->
    {ok, Pid} = coap_exchange_sup:start_child(OutPid, {PeerIP, PeerPortNo, MsgId},
        {coap_exchange_outbound, start_link,
            [MsgId, Message#coap_message.type, {self(), PeerIP, PeerPortNo}, Handler, UserData]}),
    coap_exchange:send_message(Pid, Message),
    {noreply, State#state{nextid=inc_id(MsgId)}};

% outgoing messages
handle_cast({send_message, PeerIP, PeerPortNo, BinMessage}, State=#state{socket=Socket}) ->
    %% io:fwrite("data to ~p:~p~n", [PeerIP, PeerPortNo]),
    gen_udp:send(Socket, PeerIP, PeerPortNo, BinMessage),
    {noreply, State};
handle_cast(Request, State) ->
    io:fwrite("unknown cast ~p~n", [Request]),
    {noreply, State}.

% incoming type Confirmable (0), or Non-confirmable (1)
handle_info({udp, Socket, PeerIP, PeerPortNo,
            BinMessage= <<1:2, 0:1, Type:1, _:4, _:8, MsgId:16, _Tail/bytes>>},
            State=#state{socket=Socket, handler=Handler, in_exch=InPid}) ->
    {ok, Pid} = coap_exchange_sup:start_child(InPid, {PeerIP, PeerPortNo, MsgId},
        {coap_exchange_inbound, start_link,
            [MsgId, coap_message_parser:decode_type(Type), {self(), PeerIP, PeerPortNo}, Handler]}),
    coap_exchange_inbound:handle_message(Pid, BinMessage),
    {noreply, State};
% incoming type Acknowledgement (2), or Reset (3)
handle_info({udp, Socket, PeerIP, PeerPortNo,
            BinMessage= <<1:2, 1:1, _:5, _:8, MsgId:16, _Tail/bytes>>},
            State=#state{socket=Socket, out_exch=OutPid}) ->
    case coap_exchange_sup:get_child(OutPid, {PeerIP, PeerPortNo, MsgId}) of
        {ok, Pid} -> coap_exchange_outbound:handle_message(Pid, BinMessage);
        error -> io:fwrite("ignore unexpected response~n")
    end,
    {noreply, State};
% silently ignore other versions
handle_info({udp, Socket, _, _, <<_Ver:2, _Tail/bitstring>>}, State=#state{socket=Socket}) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    io:fwrite("endpoint terminated ~p~n", [Reason]),
    ok.

% end of file
