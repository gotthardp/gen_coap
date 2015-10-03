%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(simple_storage_tests).

-export([coap_discover/2, coap_get/5, coap_delete/5]).
-export([do_storage/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gen_coap/include/coap.hrl").

coap_discover(Prefix, _Args) ->
    [{absolute, Prefix, []}].

% resource generator
coap_get(_ChId, _Prefix, [Name], _Request, _Payload) ->
    send_command({get, Name}).

coap_delete(_ChId, _Prefix, [Name], _Request, _Payload) ->
    send_command({delete, Name}).

send_command(Command) ->
    storage ! {self(), Command},
    receive
        Response -> Response
    end.

% simple storage
handle({get, Name}) ->
    case ets:lookup(resources, Name) of
        [Resource] -> {ok, Resource};
        [] -> {error, not_found}
    end;

handle({put, Name, Resource}) ->
    Before = ets:lookup(resources, Name),
    ets:insert(resources, {Name, Resource}),
    case Before of
        [] -> {ok, created};
        [_OldResource] -> {ok, chaned}
    end;

handle({delete, Name}) ->
    true = ets:delete(resources, Name),
    ok.

do_storage() ->
    resources = ets:new(resources, [set, named_table]),
    await_command(),
    ets:delete(resources).

await_command() ->
    receive
        {Pid, Command} ->
            Response = handle(Command),
            Pid ! Response,
            await_command();
        stop ->
            ok
    end.

simple_storage_test_() ->
    {setup,
        fun() ->
            register(storage, spawn(?MODULE, do_storage, [])),
            application:start(gen_coap),
            coap_server_content:add_handler([<<"storage">>], ?MODULE, undefined)
        end,
        fun(_State) ->
            application:stop(gen_coap),
            storage ! stop
        end,
        fun simple_storage_test/1}.

simple_storage_test(_State) ->
    [
    ?_assertEqual({ok, deleted, #coap_content{}}, coap_client:request(delete, "coap://127.0.0.1/storage/one")),
    ?_assertEqual({error, not_found}, coap_client:request(get, "coap://127.0.0.1/storage/one"))
    ].

% end of file
