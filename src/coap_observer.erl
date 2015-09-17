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

-include_lib("gen_coap/include/coap.hrl").

-export([start_link/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state, {sup, handler, observer, args}).

start_link(Observer, VHost, Exchange, Key) ->
    gen_server:start_link(?MODULE, [Observer, VHost, Exchange, Key], []).

notify(Resource, Request) ->
    [gen_server:cast(Pid, Request) || Pid <- pg2:get_members(Resource)].

init([Sup, Handler, Observer, Args]) ->
    apply(Handler, coap_observe, []),
    pg2:join(Args, self()),
    {ok, #state{sup=Sup, handler=Handler}}.

handle_call(_Msg, _From, State) ->
    {reply, unknown_command, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State=#state{handler=Handler}) ->
    apply(Handler, handle_info, []),
    {noreply, State}.

terminate(normal, State=#state{sup=Sup, handler=Handler, observer=Observer}) ->
    apply(Handler, handle_unobserve, []),
    % inform our supervisor that we are done
    timer:apply_after(0, supervisor, delete_child, [Sup, Observer]),
    close(State);
terminate(_Reason, State) ->
    close(State).

close(#state{handler=Handler}) ->
    apply(Handler, terminate, []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% utility functions

next_seq(Seq) ->
    if
        Seq < 16#0FFF -> Seq+1;
        true -> 0
    end.

% end of file
