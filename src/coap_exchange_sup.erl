%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% supervisor for coap_exchange_inbound and _outbound
% able to lookup worker based on an identifier
-module(coap_exchange_sup).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([start_child/3, get_child/2]).

-record(state, {dict}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_child(SupPid, Id, Start) ->
    gen_server:call(SupPid, {start_child, Id, Start}).

get_child(SupPid, Id) ->
    gen_server:call(SupPid, {get_child, Id}).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{dict=dict:new()}}.

handle_call({start_child, Id, {Mod, Fun, Args}}, _From, State=#state{dict=Dict}) ->
    case dict:find(Id, Dict) of
        {ok, Pid} ->
            {reply, {ok, Pid}, State};
        error ->
            case catch apply(Mod, Fun, Args) of
                {ok, Pid} when is_pid(Pid) ->
                    Dict2 = dict:store(Id, Pid, Dict),
                    {reply, {ok, Pid}, State#state{dict=Dict2}};
                {error, Err} ->
                    {reply, {error, Err}, State};
                What ->
                    {reply, {error, What}, State}
            end
    end;
handle_call({get_child, Id}, _From, State=#state{dict=Dict}) ->
    {reply, dict:find(Id, Dict), State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT', Child, normal}, State=#state{dict=Dict}) ->
    Dict2 = del_child(Child, Dict),
    {noreply, State#state{dict=Dict2}};
handle_info({'EXIT', Child, Reason}, State=#state{dict=Dict}) ->
    error_logger:error_msg("Handler ~p crashed: ~p~n", [Child, Reason]),
    % we don't restart for now
    Dict2 = del_child(Child, Dict),
    {noreply, State#state{dict=Dict2}};

handle_info(Msg, State) ->
    error_logger:error_msg("Supervisor received unexpected message: ~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

del_child(Child, Dict) ->
    case lists:keyfind(Child, 2, dict:to_list(Dict)) of
        {Id, FromPid} -> dict:erase(Id, Dict);
        false -> Dict
    end.

% end of file
