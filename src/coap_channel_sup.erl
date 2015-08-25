%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% stores one channel handler per endpoint
% when communication ceases the respective channel exits normally
-module(coap_channel_sup).
-behaviour(supervisor).

-export([start_link/0, start_channel/2, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

start_channel(SupPid, ChId) ->
    supervisor:start_child(SupPid,
        {ChId,
            {coap_channel, start_link, [self(), ChId]},
            transient, 5000, worker, []}).

init([]) ->
    {ok, {{one_for_one, 3, 10}, []}}.

% end of file
