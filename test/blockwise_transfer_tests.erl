%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(blockwise_transfer_tests).
-behaviour(coap_resource).

-export([coap_discover/2, coap_get/4, coap_post/4, coap_put/4, coap_delete/3,
    coap_observe/4, coap_unobserve/1, handle_info/2, coap_ack/2]).
-import(coap_test, [text_resource/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gen_coap/include/coap.hrl").

coap_discover(Prefix, _Args) ->
    [{absolute, Prefix, []}].

% resource generator
coap_get(_ChId, [<<"text">>], [Size], _Query) ->
    text_resource(binary_to_integer(Size));
coap_get(_ChId, [<<"reflect">>], [], _Query) ->
    {error, not_found}.

coap_post(_ChId, _Prefix, [], Content) ->
    {ok, content, Content}.

coap_put(_ChId, _Prefix, [], _Content) ->
    ok.

coap_delete(_ChId, _Prefix, _Suffix) -> {error, method_not_allowed}.

coap_observe(_ChId, _Prefix, _Suffix, _Ack) -> {error, method_not_allowed}.
coap_unobserve(_State) -> ok.
handle_info(_Message, State) -> {noreply, State}.
coap_ack(_Ref, State) -> {ok, State}.


% fixture is my friend
blockwise_transfer_test_() ->
    {setup,
        fun() ->
            {ok, _} = application:ensure_all_started(gen_coap),
            {ok, _} = coap_server:start_udp(coap_udp_socket),
            coap_server_registry:add_handler([<<"text">>], ?MODULE, undefined),
            coap_server_registry:add_handler([<<"reflect">>], ?MODULE, undefined)
        end,
        fun(_State) ->
            application:stop(gen_coap)
        end,
        fun blockwise_transfer/1}.

blockwise_transfer(_State) ->
    [
    % discovery
    ?_assertMatch({ok,content,#coap_content{format= <<"application/link-format">>, payload= <<"</reflect>,</text>">>}},
        coap_client:request(get, "coap://127.0.0.1/.well-known/core")),
    % resource access
    ?_assertEqual({ok,content,text_resource(128)}, coap_client:request(get, "coap://127.0.0.1/text/128")),
    ?_assertEqual({ok,content,text_resource(1024)}, coap_client:request(get, "coap://127.0.0.1/text/1024")),
    ?_assertEqual({ok,content,text_resource(1984)}, coap_client:request(get, "coap://127.0.0.1/text/1984")),
    ?_assertEqual({ok,created,#coap_content{}}, coap_client:request(put, "coap://127.0.0.1/reflect", text_resource(128))),
    ?_assertEqual({ok,created,#coap_content{}}, coap_client:request(put, "coap://127.0.0.1/reflect", text_resource(1024))),
    ?_assertEqual({ok,created,#coap_content{}}, coap_client:request(put, "coap://127.0.0.1/reflect", text_resource(1984))),
    ?_assertEqual({ok,content,text_resource(128)}, coap_client:request(post, "coap://127.0.0.1/reflect", text_resource(128))),
    ?_assertEqual({ok,content,text_resource(1024)}, coap_client:request(post, "coap://127.0.0.1/reflect", text_resource(1024))),
    ?_assertEqual({ok,content,text_resource(1984)}, coap_client:request(post, "coap://127.0.0.1/reflect", text_resource(1984)))
    ].

% end of file
