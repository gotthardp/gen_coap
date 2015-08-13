%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% handler for inbound NON and CON->ACK|RST exchanges
-module(coap_exchange_inbound).
-behaviour(gen_fsm).

-include("coap.hrl").

-export([start_link/4, handle_message/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, code_change/4, terminate/3]).
-export([init_non/2, got_non/2]).
-export([init_con/2, await_ack/2, ack_sent/2]).

-define(PROCESSING_DELAY, 2000).
-define(EXCHANGE_LIFETIME, 247000).
-define(NON_LIFETIME, 145000).

-record(state, {id, peer, handler, atimer, ack}).

start_link(MsgId, Type, Peer, Handler) ->
    gen_fsm:start_link(?MODULE, [MsgId, Type, Peer, Handler], []).

handle_message(Pid, BinMessage) ->
    gen_fsm:send_event(Pid, {in, BinMessage}).

init([MsgId, non, Peer, Handler]) ->
    gen_fsm:send_event_after(?NON_LIFETIME, expired),
    {ok, init_non, #state{id=MsgId, peer=Peer, handler=Handler}};

init([MsgId, con, Peer, Handler]) ->
    gen_fsm:send_event_after(?EXCHANGE_LIFETIME, expired),
    {ok, init_con, #state{id=MsgId, peer=Peer, handler=Handler}}.

%% --- without reliability (NON)

init_non({in, BinMessage}, Data=#state{peer=Peer, handler=undefined}) ->
    Message = coap_message_parser:decode(BinMessage),
    io:fwrite("=> ~p~n", [Message]),
    % we are acting as a server, use the request to determine the handler
    case coap_server_content:get_handler(Message) of
        undefined ->
            Ack = coap_message:response(not_found, coap_message:non(Message)),
            coap_endpoint:start_exchange(Peer, Ack),
            {next_state, got_non, Data};
        Handler ->
            Handler ! {coap_request, {self(), Peer}, Message},
            {next_state, got_non, Data#state{handler=Handler}}
    end;
init_non({in, BinMessage}, Data=#state{peer=Peer, handler=Handler}) ->
    Message = coap_message_parser:decode(BinMessage),
    io:fwrite("=> ~p~n", [Message]),
    Handler ! {coap_request, {self(), Peer}, Message},
    {next_state, got_non, Data}.

got_non({in, _Message}, Data) ->
    % ignore request retransmission
    {next_state, got_non, Data};
got_non(expired, Data)->
    {stop, normal, Data}.

%% --- reliable (CON)

init_con({in, BinMessage}, Data=#state{peer=Peer, handler=undefined}) ->
    Message = coap_message_parser:decode(BinMessage),
    io:fwrite("=> ~p~n", [Message]),
    % we are acting as a server, use the request to determine the handler
    case coap_server_content:get_handler(Message) of
        undefined ->
            io:fwrite("<- ~p~n", [Message]),
            BinAck = coap_message_parser:encode(coap_message:response(not_found, coap_message:ack(Message))),
            coap_endpoint:send_message(Peer, BinAck),
            {next_state, ack_sent, Data#state{ack=BinAck}};
        Handler ->
            Handler ! {coap_request, {self(), Peer}, Message},
            AckTimer = gen_fsm:start_timer(?PROCESSING_DELAY, ack),
            {next_state, await_ack, Data#state{handler=Handler, atimer=AckTimer}}
    end;
init_con({in, BinMessage}, Data=#state{peer=Peer, handler=Handler}) ->
    Message = coap_message_parser:decode(BinMessage),
    io:fwrite("=> ~p~n", [Message]),
    Handler ! {coap_request, {self(), Peer}, Message},
    AckTimer = gen_fsm:start_timer(?PROCESSING_DELAY, ack),
    {next_state, await_ack, Data#state{atimer=AckTimer}}.

await_ack({in, _BinMessage}, Data) ->
    % ignore request retransmission
    {next_state, await_ack, Data};
await_ack({timeout, AckTimer, ack}, Data=#state{atimer=AckTimer}) ->
    %% FIXME: send ack?
    {next_state, ack_sent, Data};

await_ack({out, Ack}, Data=#state{peer=Peer, atimer=AckTimer}) ->
    io:fwrite("<- ~p~n", [Ack]),
    gen_fsm:cancel_timer(AckTimer),
    BinAck = coap_message_parser:encode(Ack),
    coap_endpoint:send_message(Peer, BinAck),
    {next_state, ack_sent, Data#state{ack=BinAck}};
await_ack(expired, Data)->
    {stop, normal, Data}.

ack_sent({in, _BinMessage}, Data=#state{peer=Peer, ack=Ack}) ->
    % retransmit the ack
    coap_endpoint:send_message(Peer, Ack),
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
