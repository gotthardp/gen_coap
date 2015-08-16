%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% handler for outbound NON and CON->ACK|RST exchanges
-module(coap_exchange_outbound).
-behaviour(gen_fsm).

-include("coap.hrl").

-export([start_link/5, handle_message/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, code_change/4, terminate/3]).
-export([init_non/2, sent_non/2]).
-export([init_con/2, await_ack/2, ack_sent/2]).

-define(ACK_TIMEOUT, 2000).
-define(ACK_RANDOM_FACTOR, 1000). % ACK_TIMEOUT*0.5
-define(MAX_RETRANSMIT, 4).

-define(EXCHANGE_LIFETIME, 247000).
-define(NON_LIFETIME, 145000).

-record(state, {peer, handler, data, id, msg, retry_time, retry_count}).

start_link(MsgId, Type, Peer, Handler, UserData) ->
    gen_fsm:start_link(?MODULE, [MsgId, Type, Peer, Handler, UserData], []).

handle_message(Pid, BinMessage) ->
    gen_fsm:send_event(Pid, {in, BinMessage}).

init([MsgId, non, Peer, Handler, UserData]) ->
    gen_fsm:send_event_after(?NON_LIFETIME, expired),
    {ok, init_non, #state{id=MsgId, peer=Peer, handler=Handler, data=UserData}};

init([MsgId, con, Peer, Handler, UserData]) ->
    gen_fsm:send_event_after(?EXCHANGE_LIFETIME, expired),
    {ok, init_con, #state{id=MsgId, peer=Peer, handler=Handler, data=UserData}}.

%% --- without reliability (NON)

init_non({out, Message}, Data=#state{peer=Peer, id=MsgId}) ->
    Message2 = Message#coap_message{id = MsgId},
    io:fwrite("<= ~p~n", [Message2]),
    BinMessage = coap_message_parser:encode(Message2),
    coap_endpoint:send_message(Peer, BinMessage),
    {next_state, sent_non, Data}.
sent_non(expired, Data)->
    {stop, normal, Data}.

%% --- reliabile (CON)

init_con({out, Message}, Data=#state{peer=Peer, id=MsgId}) ->
    Message2 = Message#coap_message{id = MsgId},
    io:fwrite("<= ~p~n", [Message2]),
    BinMessage = coap_message_parser:encode(Message2),
    coap_endpoint:send_message(Peer, BinMessage),
    random:seed(os:timestamp()),
    Timeout = ?ACK_TIMEOUT+random:uniform(?ACK_RANDOM_FACTOR),
    {next_state, await_ack, Data#state{msg=Message2, retry_time=Timeout, retry_count=0}, Timeout}.

await_ack({in, BinAck}, Data=#state{peer=Peer, handler=Handler, data=UserData}) ->
    Ack = coap_message_parser:decode(BinAck),
    io:fwrite("-> ~p~n", [Ack]),
    Handler ! {coap_response, self(), Peer, UserData, Ack},
    {next_state, ack_sent, Data};
await_ack(timeout, Data=#state{peer=Peer, msg=Message, retry_time=Timeout, retry_count=Count}) when Count < ?MAX_RETRANSMIT ->
    BinMessage = coap_message_parser:encode(Message),
    coap_endpoint:send_message(Peer, BinMessage),
    Timeout2 = Timeout*2,
    {next_state, await_ack, Data#state{retry_time=Timeout2, retry_count=Count+1}, Timeout2};
await_ack(timeout, Data=#state{peer=Peer, handler=Handler, data=UserData, id=MsgId}) ->
    Ack = #coap_message{type=reset, id=MsgId},
    io:fwrite("[timeout] -> ~p~n", [Ack]),
    Handler ! {coap_response, self(), Peer, UserData, Ack},
    {next_state, ack_sent, Data};
await_ack(expired, Data)->
    {stop, normal, Data}.

ack_sent({in, _Ack}, Data) ->
    % ignore ack retransmission
    {next_state, ack_sent, Data};
ack_sent(expired, Data)->
    {stop, normal, Data}.

handle_event(_Event, State, Data) ->
    {next_state, State, Data}.

handle_sync_event(_Event, _From, State, Data) ->
    {next_state, State, Data}.

handle_info(_Event, State, Data) ->
    {next_state, State, Data}.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

% end of file
