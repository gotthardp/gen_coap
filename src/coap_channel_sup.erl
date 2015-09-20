%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% supervisor for a single channel
-module(coap_channel_sup).
-behaviour(supervisor).

-export([start_link/2, init/1]).

start_link(SockPid, ChId) ->
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    {ok, ReSup} = supervisor:start_child(SupPid,
        {coap_resonder_sup,
            {coap_responder_sup, start_link, []},
            permanent, infinity, supervisor, []}),
    {ok, ChPid} = supervisor:start_child(SupPid,
        {coap_channel,
            {coap_channel, start_link, [SupPid, SockPid, ChId, ReSup]},
            transient, 5000, worker, []}),
    {ok, SupPid, ChPid}.

init([]) ->
    % crash of any worker will terminate the supervisor and invoke start_link/2 again
    {ok, {{one_for_all, 0, 1}, []}}.


% end of file
