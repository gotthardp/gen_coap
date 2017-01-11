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

-export([init/6, received/2, send/2, timeout/2, awaits_response/1]).
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
% check if we can send a response
awaits_response(#state{phase=await_aack}) ->
    true;
awaits_response(_State) ->
    false.

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
    case catch coap_message_parser:decode(BinMessage) of
        #coap_message{method=Method} = Message when is_atom(Method) ->
            handle_request(Message, State);
        #coap_message{} = Message ->
            handle_response(Message, State);
        {error, _Error} ->
            % shall we sent reset back?
            ok
    end,
    next_state(got_non, State).

got_non({in, _Message}, State) ->
    % ignore request retransmission
    next_state(got_non, State).

% --- outgoing NON

out_non({out, Message}, State=#state{sock=Sock, cid=ChId}) ->
    %io:fwrite("~p <= ~p~n", [self(), Message]),
    BinMessage = coap_message_parser:encode(Message),
    Sock ! {datagram, ChId, BinMessage},
    next_state(sent_non, State).

% we may get reset
sent_non({in, BinMessage}, State)->
    case catch coap_message_parser:decode(BinMessage) of
        #coap_message{type=reset} = Message ->
            handle_error(Message, reset, State)
    end,
    next_state(got_rst, State).

got_rst({in, _BinMessage}, State)->
    next_state(got_rst, State).

% --- incoming CON->ACK|RST

in_con({in, BinMessage}, State) ->
    case catch coap_message_parser:decode(BinMessage) of
        #coap_message{method=undefined, id=MsgId} ->
            % provoked reset
            go_pack_sent(#coap_message{type=reset, id=MsgId}, State);
        #coap_message{method=Method} = Message when is_atom(Method) ->
            handle_request(Message, State),
            go_await_aack(Message, State);
        #coap_message{} = Message ->
            handle_response(Message, State),
            go_await_aack(Message, State);
        {error, Error} ->
            go_pack_sent(#coap_message{type=ack, method={error, bad_request},
                                       id=coap_message_parser:message_id(BinMessage),
                                       payload=list_to_binary(Error)}, State)
    end.

go_await_aack(Message, State) ->
    % we may need to ack the message
    BinAck = coap_message_parser:encode(coap_message:response(Message)),
    next_state(await_aack, State#state{msg=BinAck}, ?PROCESSING_DELAY).

await_aack({in, _BinMessage}, State) ->
    % ignore request retransmission
    next_state(await_aack, State);
await_aack({timeout, await_aack}, State=#state{sock=Sock, cid=ChId, msg=BinAck}) ->
    %io:fwrite("~p <- ack [application didn't respond]~n", [self()]),
    Sock ! {datagram, ChId, BinAck},
    next_state(pack_sent, State);
await_aack({out, Ack}, State) ->
    % set correct type for a piggybacked response
    Ack2 = case Ack of
        #coap_message{type=con} -> Ack#coap_message{type=ack};
        Else -> Else
    end,
    go_pack_sent(Ack2, State).

go_pack_sent(Ack, State=#state{sock=Sock, cid=ChId}) ->
    %io:fwrite("~p <- ~p~n", [self(), Ack]),
    BinAck = coap_message_parser:encode(Ack),
    Sock ! {datagram, ChId, BinAck},
    next_state(pack_sent, State#state{msg=BinAck}).

pack_sent({in, _BinMessage}, State=#state{sock=Sock, cid=ChId, msg=BinAck}) ->
    % retransmit the ack
    Sock ! {datagram, ChId, BinAck},
    next_state(pack_sent, State).

% --- outgoing CON->ACK|RST

out_con({out, Message}, State=#state{sock=Sock, cid=ChId}) ->
    %io:fwrite("~p, <= ~p~n", [self(), Message]),
    BinMessage = coap_message_parser:encode(Message),
    Sock ! {datagram, ChId, BinMessage},
    _ = rand:seed(exs1024),
    Timeout = ?ACK_TIMEOUT+rand:uniform(?ACK_RANDOM_FACTOR),
    next_state(await_pack, State#state{msg=Message, retry_time=Timeout, retry_count=0}, Timeout).

% peer ack
await_pack({in, BinAck}, State) ->
    case catch coap_message_parser:decode(BinAck) of
        #coap_message{type=ack, method=undefined} = Ack ->
            handle_ack(Ack, State);
        #coap_message{type=reset} = Ack ->
            handle_error(Ack, reset, State);
        #coap_message{} = Ack ->
            handle_response(Ack, State);
        {error, _Error} ->
            % shall we inform the receiver?
            ok
    end,
    next_state(aack_sent, State);
await_pack({timeout, await_pack}, State=#state{sock=Sock, cid=ChId, msg=Message, retry_time=Timeout, retry_count=Count}) when Count < ?MAX_RETRANSMIT ->
    BinMessage = coap_message_parser:encode(Message),
    Sock ! {datagram, ChId, BinMessage},
    Timeout2 = Timeout*2,
    next_state(await_pack, State#state{retry_time=Timeout2, retry_count=Count+1}, Timeout2);
await_pack({timeout, await_pack}, State=#state{tid={out, _MsgId}, msg=Message}) ->
    handle_error(Message, timeout, State),
    next_state(aack_sent, State).

aack_sent({in, _Ack}, State) ->
    % ignore ack retransmission
    next_state(aack_sent, State).

% utility functions

timeout_after(Time, Channel, TrId, Event) ->
    erlang:send_after(Time, Channel, {timeout, TrId, Event}).

handle_request(Message, #state{cid=ChId, channel=Channel, resp=ReSup, receiver=undefined}) ->
    %io:fwrite("~p => ~p~n", [self(), Message]),
    case coap_responder_sup:get_responder(ReSup, Message) of
        {ok, Pid} ->
            Pid ! {coap_request, ChId, Channel, undefined, Message},
            ok;
        {error, {not_found, _}} ->
            {ok, _} = coap_channel:send(Channel,
                coap_message:response({error, not_found}, Message)),
            ok
    end;
handle_request(Message, #state{cid=ChId, channel=Channel, receiver={Sender, Ref}}) ->
    %io:fwrite("~p => ~p~n", [self(), Message]),
    Sender ! {coap_request, ChId, Channel, Ref, Message},
    ok.

handle_response(Message, #state{cid=ChId, channel=Channel, receiver={Sender, Ref}}) ->
    %io:fwrite("~p -> ~p~n", [self(), Message]),
    Sender ! {coap_response, ChId, Channel, Ref, Message},
    request_complete(Channel, Message).

handle_error(Message, Error, #state{cid=ChId, channel=Channel, receiver={Sender, Ref}}) ->
    %io:fwrite("~p -> ~p~n", [self(), Message]),
    Sender ! {coap_error, ChId, Channel, Ref, Error},
    request_complete(Channel, Message).

handle_ack(_Message, #state{cid=ChId, channel=Channel, receiver={Sender, Ref}}) ->
    %io:fwrite("~p -> ~p~n", [self(), Message]),
    Sender ! {coap_ack, ChId, Channel, Ref},
    ok.

request_complete(Channel, #coap_message{token=Token, options=Options}) ->
    case proplists:get_value(observe, Options, []) of
        [] ->
            Channel ! {request_complete, Token},
            ok;
        _Else ->
            ok
    end.

% start the timer
next_state(Phase, State=#state{channel=Channel, tid=TrId, timer=undefined}, Timeout) ->
    Timer = timeout_after(Timeout, Channel, TrId, Phase),
    State#state{phase=Phase, timer=Timer};
% restart the timer
next_state(Phase, State=#state{channel=Channel, tid=TrId, timer=Timer1}, Timeout) ->
    _ = erlang:cancel_timer(Timer1),
    Timer2 = timeout_after(Timeout, Channel, TrId, Phase),
    State#state{phase=Phase, timer=Timer2}.

next_state(Phase, State=#state{timer=undefined}) ->
    State#state{phase=Phase};
next_state(Phase, State=#state{phase=Phase1, timer=Timer}) ->
    if
        % when going to another phase, the timer is cancelled
        Phase /= Phase1 ->
            _ = erlang:cancel_timer(Timer),
            ok;
        % when staying in current phase, the timer continues
        true ->
            ok
    end,
    State#state{phase=Phase, timer=undefined}.

% end of file
