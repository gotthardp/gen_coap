%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
%
-module(coap_dtls_listen).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

-export([start_socket/0, stop/1]).


start_link(Name, InPort, Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Name, InPort, Opts]).

init([Name, InPort, Opts]) ->
    {ok, ListenSocket} = ssl:listen(InPort, Opts++[binary, {protocol, dtls}, {reuseaddr, true}]),
    persistent_term:put({?MODULE, Name}, ListenSocket),
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, 60, 3600},
        [{socket,
            {coap_dtls_socket, start_link, [ListenSocket]},
            temporary, 1000, worker, [coap_dtls_socket]}
        ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.

stop(Name) ->
    case catch persistent_term:get({?MODULE, Name}) of
        {'EXIT', _} -> ok;
        Sock ->
            ssl:close(Sock),
            persistent_term:erase({?MODULE, Name})
    end.

% end of file
