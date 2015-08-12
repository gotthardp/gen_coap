%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(gen_coap).
-behaviour(application).

-export([start/0]).
-export([start/2, stop/1]).

start() ->
    start(normal, []).

start(normal, []) ->
    coap_server_sup:start_link().

stop(_State) ->
    ok.

% end of file
