%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(observe_tests).
-behaviour(coap_resource).

-export([coap_discover/2, coap_get/3, coap_post/4, coap_put/4, coap_delete/3,
    coap_observe/4, coap_unobserve/1, handle_info/2, coap_ack/2]).
-export([do_storage/0, handle/2]).
-import(coap_test, [text_resource/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gen_coap/include/coap.hrl").

% resource generator
coap_discover(Prefix, _Args) ->
    [{absolute, Prefix, []}].

coap_get(_ChId, _Prefix, []) ->
    test_utils:send_command(get).

coap_post(_ChId, _Prefix, _Suffix, _Content) ->
    {error, method_not_allowed}.

coap_put(_ChId, Prefix, [], Content) ->
    test_utils:send_command({put, Prefix, Content}).

coap_delete(_ChId, _Prefix, _Suffix) ->
    {error, method_not_allowed}.

coap_observe(_ChId, _Prefix, [], _Ack) -> {ok, undefined}.
coap_unobserve(_State) -> ok.
handle_info(_Message, State) -> {noreply, State}.
coap_ack(_Ref, State) -> {ok, State}.

% simple storage
do_storage() ->
    test_utils:await_command(?MODULE, undefined).

handle(get, State) ->
    case State of
        undefined -> {error, not_found, State};
        Resource -> {Resource, State}
    end;
handle({put, Prefix, Resource}, _State) ->
    coap_responder:notify(Prefix, Resource),
    {ok, Resource}.


% fixture is my friend
observe_test_() ->
    {setup,
        fun() ->
            register(storage, spawn(?MODULE, do_storage, [])),
            application:start(gen_coap),
            coap_server_registry:add_handler([<<"text">>], ?MODULE, undefined)
        end,
        fun(_State) ->
            application:stop(gen_coap),
            storage ! stop
        end,
        fun observe_test/1}.

observe_test(_State) ->
    [
    ?_assertEqual({error,not_found},
        coap_test:observe("coap://127.0.0.1/text")),
    ?_assertEqual({ok, created, #coap_content{}},
        coap_client:request(put, "coap://127.0.0.1/text", text_resource(<<"1">>, 2000))),
    ?_assertEqual({{ok, pid, 0, content, text_resource(<<"1">>, 2000)},
            {coap_notify, pid, 1, {ok, content}, text_resource(<<"2">>, 3000)},
            {ok, content, text_resource(<<"2">>, 3000)}},
        coap_test:observe_and_modify("coap://127.0.0.1/text", text_resource(<<"2">>, 3000)))
    ].

% end of file
