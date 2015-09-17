%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(coap_observer_sup).
-behaviour(supervisor).

-export([start_link/0, start_observer/2, restart_observer/2, stop_observer/2, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

start_observer(SupPid, Observer) ->
    supervisor:start_child(SupPid,
        {Observer,
            {coap_observer, start_link, [SupPid, Observer]},
            transient, 5000, worker, []}).

restart_observer(SupPid, Observer) ->
    case start_observer(SupPid, Observer) of
        {error, {already_started, ObserverPid}} ->
            ok = supervisor:terminate_child(SupPid, ObserverPid),
            ok = supervisor:delete_child(SupPid, Observer),
            start_observer(SupPid, Observer);
        Otherwise ->
            Otherwise
    end.

stop_observer(SupPid, Observer) ->
    ok = supervisor:terminate_child(SupPid, Observer),
    ok = supervisor:delete_child(SupPid, Observer).

init([]) ->
    {ok, {{one_for_one, 3, 10}, []}}.

% end of file
