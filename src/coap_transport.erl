%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% NON and CON->ACK|RST message transmission
% handles message retransmission and de-duplication
-module(coap_transport).

-export([init/6, received/2, send/2, timeout/2]).
-export([idle/2, got_non/2, sent_non/2, got_rst/2, await_aack/2, pack_sent/2, await_pack/2, aack_sent/2]).

-define(ACK_TIMEOUT, 2000).
-define(ACK_RANDOM_FACTOR, 1000). % ACK_TIMEOUT*0.5
-define(MAX_RETRANSMIT, 4).

-define(PROCESSING_DELAY, 1000). % standard allows 2000
-define(EXCHANGE_LIFETIME, 247000).
-define(NON_LIFETIME, 145000).

-record(state, {phase, sock, cid, channel, tid, resp, receiver, msg, timer, retry_time, retry_count}).

-include("coap.hrl").

init(Sock, ChId, Channel, TrId, ReSup, Receiver) ->
    #state{phase=idle, sock=Sock, cid=ChId, channel=Channel, tid=TrId, resp=ReSup, receiver=Receiver}.
% process incoming message
received(BinMessage, State=#state{phase=Phase}) ->
    ?MODULE:Phase({in, BinMessage}, State).
% process outgoing message
send(Message, State=#state{phase=Phase}) ->
    ?MODULE:Phase({out, Message}, State).
% when the transport expires remove terminate the state
timeout(transport, _State) ->
    undefined;
% process timeout
timeout(Event, State=#state{phase=Phase}) ->
    ?MODULE:Phase({timeout, Event}, State).

% ->NON
idle(Msg={in, <<1:2, 1:2, _:12, _Tail/bytes>>}, State=#state{channel=Channel, tid=TrId}) ->
    timeout_after(?NON_LIFETIME, Channel, TrId, transport),
    in_non(Msg, State);
% ->CON
idle(Msg={in, <<1:2, 0:2, _:12, _Tail/bytes>>}, State=#state{channel=Channel, tid=TrId}) ->
    timeout_after(?EXCHANGE_LIFETIME, Channel, TrId, transport),
    in_con(Msg, State);
% NON->
idle(Msg={out, #coap_message{type=non}}, State=#state{channel=Channel, tid=TrId}) ->
    timeout_after(?NON_LIFETIME, Channel, TrId, transport),
    out_non(Msg, State);
% CON->
idle(Msg={out, #coap_message{type=con}}, State=#state{channel=Channel, tid=TrId}) ->
    timeout_after(?EXCHANGE_LIFETIME, Channel, TrId, transport),
    out_con(Msg, State).

% --- incoming NON

in_non({in, BinMessage}, State) ->
    Message = coap_message_parser:decode(BinMessage),
    io:fwrite("=> ~p~n", [Message]),
    case Message of
        #coap_message{method=Method} when is_atom(Method) ->
            handle_request(Message, State);
        #coap_message{} ->
            handle_response(Message, State)
    end,
    next_state(got_non, State).

got_non({in, _Message}, State) ->
    % ignore request retransmission
    next_state(got_non, State).

% --- outgoing NON

out_non({out, Message}, State=#state{sock=Sock, cid=ChId}) ->
    io:fwrite("<= ~p~n", [Message]),
    BinMessage = coap_message_parser:encode(Message),
    Sock ! {datagram, ChId, BinMessage},
    next_state(sent_non, State).

% we may get reset
sent_non({in, BinMessage}, State)->
    Message = coap_message_parser:decode(BinMessage),
    io:fwrite("-> ~p~n", [Message]),
    case Message of
        #coap_message{type=reset} ->
            handle_error(Message, reset, State)
    end,
    next_state(got_rst, State).

got_rst({in, _BinMessage}, State)->
    next_state(got_rst, State).

% --- incoming CON->ACK|RST

in_con({in, BinMessage}, State=#state{channel=Channel}) ->
    Message = coap_message_parser:decode(BinMessage),
    io:fwrite("=> ~p~n", [Message]),
    case Message of
        #coap_message{method=undefined, id=MsgId} ->
            % provoked reset
            coap_channel:send(Channel, #coap_message{type=reset, id=MsgId});
        #coap_message{method=Method} when is_atom(Method) ->
            handle_request(Message, State);
        #coap_message{} ->
            handle_response(Message, State)
    end,
    % we may need to ack the message
    BinAck = coap_message_parser:encode(coap_message:response(Message)),
    next_state(await_aack, State#state{msg=BinAck}, ?PROCESSING_DELAY).

await_aack({in, _BinMessage}, State) ->
    % ignore request retransmission
    next_state(await_aack, State);
await_aack({timeout, await_aack}, State=#state{sock=Sock, cid=ChId, msg=BinAck}) ->
    io:fwrite("<- ack [application didn't respond]~n"),
    Sock ! {datagram, ChId, BinAck},
    next_state(pack_sent, State);
await_aack({out, Ack}, State=#state{sock=Sock, cid=ChId}) ->
    io:fwrite("<- ~p~n", [Ack]),
    BinAck = coap_message_parser:encode(Ack),
    Sock ! {datagram, ChId, BinAck},
    next_state(pack_sent, State#state{msg=BinAck}).

pack_sent({in, _BinMessage}, State=#state{sock=Sock, cid=ChId, msg=BinAck}) ->
    % retransmit the ack
    Sock ! {datagram, ChId, BinAck},
    next_state(pack_sent, State).

% --- outgoing CON->ACK|RST

out_con({out, Message}, State=#state{sock=Sock, cid=ChId}) ->
    io:fwrite("<= ~p~n", [Message]),
    BinMessage = coap_message_parser:encode(Message),
    Sock ! {datagram, ChId, BinMessage},
    random:seed(os:timestamp()),
    Timeout = ?ACK_TIMEOUT+random:uniform(?ACK_RANDOM_FACTOR),
    next_state(await_pack, State#state{msg=Message, retry_time=Timeout, retry_count=0}, Timeout).

% peer ack
await_pack({in, BinAck}, State) ->
    Ack = coap_message_parser:decode(BinAck),
    io:fwrite("-> ~p~n", [Ack]),
    case Ack of
        #coap_message{type=ack, method=undefined} ->
            handle_ack(Ack, State);
        #coap_message{type=reset} ->
            handle_error(Ack, reset, State);
        #coap_message{} ->
            handle_response(Ack, State)
    end,
    next_state(aack_sent, State);
await_pack({timeout, await_pack}, State=#state{sock=Sock, cid=ChId, msg=Message, retry_time=Timeout, retry_count=Count}) when Count < ?MAX_RETRANSMIT ->
    BinMessage = coap_message_parser:encode(Message),
    Sock ! {datagram, ChId, BinMessage},
    Timeout2 = Timeout*2,
    next_state(await_pack, State#state{retry_time=Timeout2, retry_count=Count+1}, Timeout2);
await_pack({timeout, await_pack}, State=#state{tid={out, MsgId}, msg=Message}) ->
    io:fwrite("-> timeout ~p~n", [MsgId]),
    handle_error(Message, timeout, State),
    next_state(aack_sent, State).

aack_sent({in, _Ack}, State) ->
    % ignore ack retransmission
    next_state(aack_sent, State).

% utility functions

timeout_after(Time, Channel, TrId, Event) ->
    erlang:send_after(Time, Channel, {timeout, TrId, Event}).

handle_request(Message, #state{cid=ChId, channel=Channel, resp=ReSup, receiver=undefined}) ->
    case coap_responder_sup:get_responder(ReSup, Message) of
        {ok, Pid} ->
            Pid ! {coap_request, ChId, Channel, undefined, Message};
        {error, {not_found, _}} ->
            coap_channel:send(Channel,
                coap_message:response({error, not_found}, Message))
    end;
handle_request(Message, #state{cid=ChId, channel=Channel, receiver={Sender, Ref}}) ->
    Sender ! {coap_request, ChId, Channel, Ref, Message}.

handle_response(Message, #state{cid=ChId, channel=Channel, receiver={Sender, Ref}}) ->
    Sender ! {coap_response, ChId, Channel, Ref, Message},
    request_complete(Channel, Message).

handle_error(Message=#coap_message{id=MsgId}, Error, #state{cid=ChId, channel=Channel, receiver={Sender, Ref}}) ->
    Sender ! {coap_error, ChId, Channel, Ref, Error},
    request_complete(Channel, Message).

handle_ack(_Message, #state{cid=ChId, channel=Channel, receiver={Sender, Ref}}) ->
    Sender ! {coap_ack, ChId, Channel, Ref}.

request_complete(Channel, #coap_message{token=Token, options=Options}) ->
    case proplists:get_value(observe, Options, []) of
        [] -> Channel ! {request_complete, Token};
        _Else -> ok
    end.

% start the timer
next_state(Phase, State=#state{channel=Channel, tid=TrId, timer=undefined}, Timeout) ->
    Timer = timeout_after(Timeout, Channel, TrId, Phase),
    State#state{phase=Phase, timer=Timer};
% restart the timer
next_state(Phase, State=#state{channel=Channel, tid=TrId, timer=Timer1}, Timeout) ->
    erlang:cancel_timer(Timer1),
    Timer2 = timeout_after(Timeout, Channel, TrId, Phase),
    State#state{phase=Phase, timer=Timer2}.

next_state(Phase, State=#state{timer=undefined}) ->
    State#state{phase=Phase};
next_state(Phase, State=#state{phase=Phase1, timer=Timer}) ->
    if
        % when going to another phase, the timer is cancelled
        Phase /= Phase1 -> erlang:cancel_timer(Timer);
        % when staying in current phase, the timer continues
        true -> ok
    end,
    State#state{phase=Phase, timer=undefined}.

% end of file
