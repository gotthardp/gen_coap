%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% socket pair, identified by a 2-tuple of local and remote socket addresses
% stores state for a given endpoint
-module(coap_channel).
-behaviour(gen_server).

-export([start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([ping/1, send/2, send_request/3, send_message/3, send_response/3, close/1]).

-define(VERSION, 1).
-define(MAX_MESSAGE_ID, 65535). % 16-bit number

-record(state, {sup, sock, cid, tokens, trans, nextmid, res, rescnt}).

-include("coap.hrl").

start_link(SupPid, SockPid, ChId, ReSup) ->
    gen_server:start_link(?MODULE, [SupPid, SockPid, ChId, ReSup], []).

ping(Channel) ->
    send_message(Channel, make_ref(), #coap_message{type=con}).

send(Channel, Message=#coap_message{type=Type, method=Method})
        when is_tuple(Method); Type==ack; Type==reset ->
    send_response(Channel, make_ref(), Message);
send(Channel, Message=#coap_message{}) ->
    send_request(Channel, make_ref(), Message).

send_request(Channel, Ref, Message) ->
    gen_server:cast(Channel, {send_request, Message, {self(), Ref}}),
    {ok, Ref}.
send_message(Channel, Ref, Message) ->
    gen_server:cast(Channel, {send_message, Message, {self(), Ref}}),
    {ok, Ref}.
send_response(Channel, Ref, Message) ->
    gen_server:cast(Channel, {send_response, Message, {self(), Ref}}),
    {ok, Ref}.

close(Pid) ->
    gen_server:cast(Pid, shutdown).

init([SupPid, SockPid, ChId, ReSup]) ->
    % we want to get called upon termination
    process_flag(trap_exit, true),
    {ok, #state{sup=SupPid, sock=SockPid, cid=ChId, tokens=dict:new(),
        trans=dict:new(), nextmid=first_mid(), res=ReSup, rescnt=0}}.

handle_call(_Unknown, _From, State) ->
    {reply, unknown_call, State}.

% outgoing CON(0) or NON(1) request
handle_cast({send_request, Message, Receiver}, State) ->
    transport_new_request(Message, Receiver, State);
% outgoing CON(0) or NON(1)
handle_cast({send_message, Message, Receiver}, State) ->
    transport_new_message(Message, Receiver, State);
% outgoing response, either CON(0) or NON(1), piggybacked ACK(2) or RST(3)
handle_cast({send_response, Message, Receiver}, State) ->
    transport_response(Message, Receiver, State);
handle_cast(shutdown, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    io:fwrite("coap_channel unknown cast ~p~n", [Request]),
    {noreply, State}.

transport_new_request(Message, Receiver, State=#state{tokens=Tokens}) ->
    Token = crypto:strong_rand_bytes(4), % shall be at least 32 random bits
    Tokens2 = dict:store(Token, Receiver, Tokens),
    transport_new_message(Message#coap_message{token=Token}, Receiver, State#state{tokens=Tokens2}).

transport_new_message(Message, Receiver, State=#state{nextmid=MsgId}) ->
    transport_message({out, MsgId}, Message#coap_message{id=MsgId}, Receiver, State#state{nextmid=next_mid(MsgId)}).

transport_message(TrId, Message, Receiver, State) ->
    update_state(State, TrId,
        coap_transport:send(Message, create_transport(TrId, Receiver, State))).

transport_response(Message=#coap_message{id=MsgId}, Receiver, State=#state{trans=Trans}) ->
    case dict:find({in, MsgId}, Trans) of
        {ok, TrState} ->
            case coap_transport:awaits_response(TrState) of
                true ->
                    update_state(State, {in, MsgId},
                        coap_transport:send(Message, TrState));
                false ->
                    transport_new_message(Message, Receiver, State)
            end;
        error ->
            transport_new_message(Message, Receiver, State)
    end.

% incoming CON(0) or NON(1) request
handle_info({datagram, BinMessage= <<?VERSION:2, 0:1, _:1, _TKL:4, 0:3, _CodeDetail:5, MsgId:16, _/bytes>>}, State) ->
    TrId = {in, MsgId},
    update_state(State, TrId,
        coap_transport:received(BinMessage, create_transport(TrId, undefined, State)));
% incoming CON(0) or NON(1) response
handle_info({datagram, BinMessage= <<?VERSION:2, 0:1, _:1, TKL:4, _Code:8, MsgId:16, Token:TKL/bytes, _/bytes>>},
        State=#state{sock=Sock, cid=ChId, tokens=Tokens, trans=Trans}) ->
    TrId = {in, MsgId},
    case dict:find(TrId, Trans) of
        {ok, TrState} ->
            update_state(State, TrId, coap_transport:received(BinMessage, TrState));
        error ->
            case dict:find(Token, Tokens) of
                {ok, Receiver} ->
                    update_state(State, TrId,
                        coap_transport:received(BinMessage, init_transport(TrId, Receiver, State)));
                error ->
                    % token was not recognized
                    BinReset = coap_message_parser:encode(#coap_message{type=reset, id=MsgId}),
                    io:fwrite("<- reset~n"),
                    Sock ! {datagram, ChId, BinReset}
            end
    end;
% incoming ACK(2) or RST(3) to a request or response
handle_info({datagram, BinMessage= <<?VERSION:2, _:2, _TKL:4, _Code:8, MsgId:16, _/bytes>>},
        State=#state{trans=Trans}) ->
    TrId = {out, MsgId},
    update_state(State, TrId,
        case dict:find(TrId, Trans) of
            error -> undefined; % ignore unexpected responses
            {ok, TrState} -> coap_transport:received(BinMessage, TrState)
        end);
% silently ignore other versions
handle_info({datagram, <<Ver:2, _/bytes>>}, State) when Ver /= ?VERSION ->
    {noreply, State};
handle_info({timeout, TrId, Event}, State=#state{trans=Trans}) ->
    update_state(State, TrId,
        case dict:find(TrId, Trans) of
            error -> undefined; % ignore unexpected responses
            {ok, TrState} -> coap_transport:timeout(Event, TrState)
        end);
handle_info({request_complete, Token}, State=#state{tokens=Tokens}) ->
    Tokens2 = dict:erase(Token, Tokens),
    purge_state(State#state{tokens=Tokens2});
handle_info({responder_started}, State=#state{rescnt=Count}) ->
    purge_state(State#state{rescnt=Count+1});
handle_info({responder_completed}, State=#state{rescnt=Count}) ->
    purge_state(State#state{rescnt=Count-1});
handle_info(Info, State) ->
    io:fwrite("unexpected massage ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{sup=SupPid, sock=SockPid, cid=ChId}) ->
    %io:fwrite("channel ~p finished ~p~n", [ChId, Reason]),
    SockPid ! {terminated, SupPid, ChId},
    ok.


first_mid() ->
    _ = rand:seed(exs1024),
    rand:uniform(?MAX_MESSAGE_ID).

next_mid(MsgId) ->
    if
        MsgId < ?MAX_MESSAGE_ID -> MsgId + 1;
        true -> 1 % or 0?
    end.

create_transport(TrId, Receiver, State=#state{trans=Trans}) ->
    case dict:find(TrId, Trans) of
        {ok, TrState} -> TrState;
        error -> init_transport(TrId, Receiver, State)
    end.

init_transport(TrId, undefined, #state{sock=Sock, cid=ChId, res=ReSup}) ->
    coap_transport:init(Sock, ChId, self(), TrId, ReSup, undefined);
init_transport(TrId, Receiver, #state{sock=Sock, cid=ChId}) ->
    coap_transport:init(Sock, ChId, self(), TrId, undefined, Receiver).

update_state(State=#state{trans=Trans}, TrId, undefined) ->
    Trans2 = dict:erase(TrId, Trans),
    purge_state(State#state{trans=Trans2});
update_state(State=#state{trans=Trans}, TrId, TrState) ->
    Trans2 = dict:store(TrId, TrState, Trans),
    {noreply, State#state{trans=Trans2}}.

purge_state(State=#state{tokens=Tokens, trans=Trans, rescnt=Count}) ->
    case dict:size(Tokens)+dict:size(Trans)+Count of
        0 -> {stop, normal, State};
        _Else -> {noreply, State}
    end.

% end of file
