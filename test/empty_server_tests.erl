%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(empty_server_tests).
-behaviour(coap_resource).

-export([coap_discover/2, coap_get/4, coap_post/4, coap_put/4, coap_delete/3,
    coap_observe/4, coap_unobserve/1, handle_info/2, coap_ack/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gen_coap/include/coap.hrl").

coap_discover(_Prefix, _Args) -> [].

coap_get(_ChId, _Prefix, _Suffix, _Query) -> {error, method_not_allowed}.
coap_post(_ChId, _Prefix, _Suffix, _Content) -> {error, method_not_allowed}.
coap_put(_ChId, _Prefix, _Suffix, _Content) -> {error, method_not_allowed}.
coap_delete(_ChId, _Prefix, _Suffix) -> {error, method_not_allowed}.

coap_observe(_ChId, _Prefix, _Suffix, _Ack) -> {error, method_not_allowed}.
coap_unobserve(_State) -> ok.
handle_info(_Message, State) -> {noreply, State}.
coap_ack(_Ref, State) -> {ok, State}.

% fixture is my friend
empty_server_test_() ->
    {setup,
        fun() ->
            {ok, _} = application:ensure_all_started(gen_coap),
            {ok, _} = coap_server:start_udp(coap_udp_socket)
        end,
        fun(_State) ->
            application:stop(gen_coap)
        end,
        fun empty_server/1}.

empty_server(_State) ->
    [
    % provoked reset
    ?_assertEqual(ok, coap_client:ping("coap://127.0.0.1")),
    % discovery
    ?_assertEqual({error, not_found}, coap_client:request(get, "coap://127.0.0.1")),
    ?_assertEqual({error, not_found}, coap_client:request(get, "coap://127.0.0.1/.well-known")),
    ?_assertMatch({ok, content, #coap_content{payload= <<>>}}, coap_client:request(get, "coap://127.0.0.1/.well-known/core")),
    % other methods
    ?_assertEqual({error,method_not_allowed}, coap_client:request(post, "coap://127.0.0.1/.well-known/core")),
    ?_assertEqual({error,method_not_allowed}, coap_client:request(put, "coap://127.0.0.1/.well-known/core")),
    ?_assertEqual({error,method_not_allowed}, coap_client:request(delete, "coap://127.0.0.1/.well-known/core"))
    ].


unknown_handler_test_() ->
    {setup,
        fun() ->
            {ok, _} = application:ensure_all_started(gen_coap),
            {ok, _} = coap_server:start_udp(coap_udp_socket),
            coap_server_registry:add_handler([<<"unknown">>], unknown_module, undefined)
        end,
        fun(_State) ->
            application:stop(gen_coap)
        end,
        fun unknown_handler/1}.

unknown_handler(_State) ->
    [
    % provoked reset
    ?_assertMatch({ok, content, #coap_content{payload= <<>>}}, coap_client:request(get, "coap://127.0.0.1/.well-known/core")),
    ?_assertEqual({error,internal_server_error}, coap_client:request(get, "coap://127.0.0.1/unknown"))
    ].

% end of file
