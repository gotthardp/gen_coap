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
-export([start_udp/1, start_udp/2, stop_udp/1, start_dtls/2, start_dtls/3, stop_dtls/1, channel_sup/1]).

-include("coap.hrl").

start() ->
    start(normal, []).

start(normal, []) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_Pid) ->
    ok.


init([]) ->
    {ok, {{one_for_all, 3, 10}, [
        {coap_server_registry,
            {coap_server_registry, start_link, []},
            permanent, 5000, worker, []},
        {coap_channel_sup_sup,
            {coap_channel_sup_sup, start_link, []},
            permanent, infinity, supervisor, []}
    ]}}.

start_udp(Name) ->
    start_udp(Name, ?DEFAULT_COAP_PORT).

start_udp(Name, UdpPort) ->
    supervisor:start_child(?MODULE,
        {Name,
            {coap_udp_socket, start_link, [UdpPort, whereis(?MODULE)]},
            transient, 5000, worker, []}).

stop_udp(Name) ->
    supervisor:terminate_child(?MODULE, Name),
    supervisor:delete_child(?MODULE, Name).


start_dtls(Name, DtlsOpts) ->
    start_dtls(Name, ?DEFAULT_COAPS_PORT, DtlsOpts).

start_dtls(Name, DtlsPort, DtlsOpts) ->
    supervisor:start_child(?MODULE,
        {Name,
            {coap_dtls_listen, start_link, [DtlsPort, DtlsOpts]},
            transient, 5000, worker, []}).

stop_dtls(Name) ->
    supervisor:terminate_child(?MODULE, Name),
    supervisor:delete_child(?MODULE, Name).


channel_sup(SupPid) -> child(SupPid, coap_channel_sup_sup).

child(SupPid, Id) ->
    [Pid] = [Pid || {Id1, Pid, _, _} <- supervisor:which_children(SupPid),
        Id1 =:= Id],
    Pid.

% end of file
