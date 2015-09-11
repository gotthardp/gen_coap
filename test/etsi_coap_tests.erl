%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% tests per ETSI CoAP test specifications (and few more)
-module(etsi_coap_tests).

-include_lib("eunit/include/eunit.hrl").

% fixture is my friend
empty_server_test_() ->
    {setup,
        fun() ->
            coap_server:start()
        end,
        fun(ServerPid) ->
            coap_server:stop(ServerPid)
        end,
        fun empty_server/1}.

empty_server(_ServerPid) ->
    [
    % discovery
    ?_assertEqual({error, not_found}, coap_client:request(get, "coap://127.0.0.1")),
    ?_assertEqual({error, not_found}, coap_client:request(get, "coap://127.0.0.1/")),
    ?_assertEqual({error, not_found}, coap_client:request(get, "coap://127.0.0.1/.well-known")),
    ?_assertEqual({ok, content}, coap_client:request(get, "coap://127.0.0.1/.well-known/core")),
    % other methods
    ?_assertEqual({error,method_not_allowed}, coap_client:request(post, "coap://127.0.0.1/.well-known/core")),
    ?_assertEqual({error,method_not_allowed}, coap_client:request(put, "coap://127.0.0.1/.well-known/core")),
    ?_assertEqual({error,method_not_allowed}, coap_client:request(delete, "coap://127.0.0.1/.well-known/core"))
    ].

% end of file
