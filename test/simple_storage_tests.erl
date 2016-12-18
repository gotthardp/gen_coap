%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(simple_storage_tests).
-behaviour(coap_resource).

-export([coap_discover/2, coap_get/4, coap_post/4, coap_put/4, coap_delete/3,
    coap_observe/4, coap_unobserve/1, handle_info/2, coap_ack/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gen_coap/include/coap.hrl").

-define(NOT_IMPLEMENTED, <<"Not implemented">>).

% resource operations
coap_discover(Prefix, _Args) ->
    [{absolute, Prefix, []}].

coap_get(_ChId, _Prefix, [Name], _Query) ->
    case mnesia:dirty_read(resources, Name) of
        [{resources, Name, Resource}] -> Resource;
        [] -> {error, not_found}
    end.

coap_post(_ChId, _Prefix, _Suffix, _Content) ->
    {error, method_not_allowed, ?NOT_IMPLEMENTED}.

coap_put(_ChId, _Prefix, [Name], Content) ->
    mnesia:dirty_write(resources, {resources, Name, Content}).

coap_delete(_ChId, _Prefix, [Name]) ->
    mnesia:dirty_delete(resources, Name).

coap_observe(_ChId, _Prefix, _Suffix, _Ack) -> {error, method_not_allowed}.
coap_unobserve(_State) -> ok.
handle_info(_Message, State) -> {noreply, State}.
coap_ack(_Ref, State) -> {ok, State}.


% fixture is my friend
simple_storage_test_() ->
    {setup,
        fun() ->
            ok = application:start(mnesia),
            {atomic, ok} = mnesia:create_table(resources, []),
            {ok, _} = application:ensure_all_started(gen_coap),
            {ok, _} = coap_server:start_udp(coap_udp_socket),
            coap_server_registry:add_handler([<<"storage">>], ?MODULE, undefined)
        end,
        fun(_State) ->
            application:stop(gen_coap),
            application:stop(mnesia)
        end,
        fun simple_storage_test/1}.

simple_storage_test(_State) ->
    [
    ?_assertEqual({ok, deleted, #coap_content{}},
        coap_client:request(delete, "coap://127.0.0.1/storage/one")),

    ?_assertEqual({error, not_found},
        coap_client:request(get, "coap://127.0.0.1/storage/one")),

    ?_assertEqual({error, method_not_allowed, #coap_content{payload=?NOT_IMPLEMENTED}},
        coap_client:request(post, "coap://127.0.0.1/storage/one", #coap_content{})),

    ?_assertEqual({ok, created, #coap_content{}},
        coap_client:request(put, "coap://127.0.0.1/storage/one",
            #coap_content{etag= <<"1">>, payload= <<"1">>}, [{if_none_match, true}])),

    ?_assertEqual({error,precondition_failed},
        coap_client:request(put, "coap://127.0.0.1/storage/one",
            #coap_content{etag= <<"1">>, payload= <<"1">>}, [{if_none_match, true}])),

    ?_assertEqual({ok, content, #coap_content{etag= <<"1">>, payload= <<"1">>}},
        coap_client:request(get, "coap://127.0.0.1/storage/one")),

    ?_assertEqual({ok, valid, #coap_content{etag= <<"1">>}},
        coap_client:request(get, "coap://127.0.0.1/storage/one",
            #coap_content{}, [{etag, [<<"1">>]}])),

    ?_assertEqual({ok, changed, #coap_content{}},
        coap_client:request(put, "coap://127.0.0.1/storage/one",
            #coap_content{etag= <<"2">>, payload= <<"2">>})),

    ?_assertEqual({ok, content, #coap_content{etag= <<"2">>, payload= <<"2">>}},
        coap_client:request(get, "coap://127.0.0.1/storage/one")),

    ?_assertEqual({ok, content, #coap_content{etag= <<"2">>, payload= <<"2">>}},
        coap_client:request(get, "coap://127.0.0.1/storage/one",
            #coap_content{}, [{etag, [<<"1">>]}])),

    % observe existing resource when coap_observe is not implemented
    ?_assertEqual({ok, content, #coap_content{etag= <<"2">>, payload= <<"2">>}},
        coap_observer:observe("coap://127.0.0.1/storage/one")),

    ?_assertEqual({ok, valid, #coap_content{etag= <<"2">>}},
        coap_client:request(get, "coap://127.0.0.1/storage/one",
            #coap_content{}, [{etag, [<<"1">>, <<"2">>]}])),

    ?_assertEqual({ok, deleted, #coap_content{}},
        coap_client:request(delete, "coap://127.0.0.1/storage/one")),

    ?_assertEqual({error, not_found},
        coap_client:request(get, "coap://127.0.0.1/storage/one")),

    % observe non-existing resource when coap_observe is not implemented
    ?_assertEqual({error, not_found},
        coap_observer:observe("coap://127.0.0.1/storage/one"))
    ].

% end of file
