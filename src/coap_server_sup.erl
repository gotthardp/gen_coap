%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(coap_server_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(DEFAULT_COAP_PORT, 5683).

start_link() ->
    start_link(?DEFAULT_COAP_PORT).

start_link(InPort) ->
    {ok, SupPid} = supervisor:start_link(?MODULE, _Arg = []),
    InExPid = child(SupPid, coap_exchange_inbound),
    OutExPid = child(SupPid, coap_exchange_outbound),
    supervisor:start_child(SupPid, 
        {coap_endpoint, {coap_endpoint, start_link, [InPort, undefined, InExPid, OutExPid]},
            permanent, 20000, worker, []}).

init([]) ->
    {ok, {{one_for_all, 3, 10},
        [{coap_server_content, {coap_server_content, start_link, []}, permanent, 20000, worker, []},
        {coap_exchange_inbound, {coap_exchange_sup, start_link, []}, permanent, 20000, supervisor, []},
        {coap_exchange_outbound, {coap_exchange_sup, start_link, []}, permanent, 20000, supervisor, []}]
    }}.

child(Sup, Id) ->
    [Pid] = [Pid || {Id1, Pid, _, _} <- supervisor:which_children(Sup),
        Id1 =:= Id],
    Pid.

% end of file
