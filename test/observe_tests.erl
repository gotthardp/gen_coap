%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(observe_tests).

-export([coap_discover/2, coap_get/3, coap_observe/3, coap_put/4]).
-export([do_storage/0, handle/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gen_coap/include/coap.hrl").

% resource generator
coap_discover(Prefix, _Args) ->
    [{absolute, Prefix, []}].

coap_get(_ChId, _Prefix, []) ->
    test_utils:send_command(get).

coap_observe(_ChId, Prefix, []) ->
    timer:apply_after(500, coap_responder, notify, [Prefix, {ok, #coap_content{payload= <<"2">>}}]),
    ok.

coap_put(_ChId, _Prefix, [], Content) ->
    test_utils:send_command({put, Content}).

% simple storage
do_storage() ->
    test_utils:await_command(?MODULE, undefined).

handle(get, State) ->
    case State of
        undefined -> {error, not_found, State};
        Resource -> {ok, Resource, State}
    end;
handle({put, Resource}, _State) ->
    {ok, Resource}.


% fixture is my friend
observe_test_() ->
    {setup,
        fun() ->
            register(storage, spawn(?MODULE, do_storage, [])),
            application:start(gen_coap),
            coap_server_content:add_handler([<<"text">>], ?MODULE, undefined)
        end,
        fun(_State) ->
            application:stop(gen_coap),
            storage ! stop
        end,
        fun observe_test/1}.

observe_and_wait(Uri) ->
    case coap_observer:observe(Uri) of
        Subscribed = {ok, _Code, _Content} ->
            Notify = receive
                {coap_notify, {ok, Code}, Content} ->
                    {ok, Code, Content};
                {coap_notify, {error, Code}} ->
                    {error, Code}
            end,
            {Subscribed, Notify};
        NotSubscribed ->
            NotSubscribed
    end.

observe_test(_State) ->
    [
    ?_assertEqual({error,not_found},
        observe_and_wait("coap://127.0.0.1/text")),
    ?_assertEqual({ok, created, #coap_content{}},
        coap_client:request(put, "coap://127.0.0.1/text", #coap_content{payload= <<"1">>})),
    ?_assertEqual({{ok, content, #coap_content{payload= <<"1">>}},
            {ok, content, #coap_content{payload= <<"2">>}}},
        observe_and_wait("coap://127.0.0.1/text"))
    ].

% end of file
