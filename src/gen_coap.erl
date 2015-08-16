%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
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
