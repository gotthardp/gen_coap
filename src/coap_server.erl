%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% CoAP server application
% supervisor for content registry, listening socket and channel supervisor
-module(coap_server).

-behaviour(application).
-export([start/0, start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).
-export([channel_sup/1]).

-define(DEFAULT_COAP_PORT, 5683).

start() ->
    start(normal, []).

start(normal, []) ->
    supervisor:start_link(?MODULE, [?DEFAULT_COAP_PORT]).

stop(Pid) ->
    exit(Pid, shutdown).


init([InPort]) ->
    {ok, {{one_for_all, 3, 10}, [
        {coap_server_content,
            {coap_server_content, start_link, []},
            permanent, 5000, worker, []},
        {coap_channel_sup_sup,
            {coap_channel_sup_sup, start_link, []},
            permanent, infinity, supervisor, []},
        {coap_udp_socket,
            % for convenience register the main worker under the application name
            {coap_udp_socket, start_link, [{local, ?MODULE}, InPort, self()]},
            permanent, 5000, worker, []}
    ]}}.

channel_sup(SupPid) -> child(SupPid, coap_channel_sup_sup).

child(SupPid, Id) ->
    [Pid] = [Pid || {Id1, Pid, _, _} <- supervisor:which_children(SupPid),
        Id1 =:= Id],
    Pid.

% end of file
