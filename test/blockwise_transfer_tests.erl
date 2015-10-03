%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(blockwise_transfer_tests).

-export([coap_discover/2, coap_get/5]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("gen_coap/include/coap.hrl").

coap_discover(Prefix, _Args) ->
    [{absolute, Prefix, []}].

% resource generator
coap_get(_ChId, _Prefix, [Size], _Request, _Payload) ->
    {ok, text_resource(binary_to_integer(Size))}.

text_resource(Size) ->
    #coap_content{format= <<"text/plain">>, payload=large_binary(Size, <<"X">>)}.

large_binary(Size, Acc) when Size > 2*byte_size(Acc) ->
    large_binary(Size, <<Acc/binary, Acc/binary>>);
large_binary(Size, Acc) ->
    Sup = binary:part(Acc, 0, Size-byte_size(Acc)),
    <<Acc/binary, Sup/binary>>.

% fixture is my friend
blockwise_transfer_test_() ->
    {setup,
        fun() ->
            application:start(gen_coap),
            coap_server_content:add_handler([<<"text">>], ?MODULE, undefined)
        end,
        fun(_State) ->
            application:stop(gen_coap)
        end,
        fun blockwise_transfer/1}.

blockwise_transfer(_State) ->
    [
    % discovery
    ?_assertMatch({ok,content,#coap_content{format= <<"application/link-format">>, payload= <<"</text>">>}},
        coap_client:request(get, "coap://127.0.0.1/.well-known/core")),
    % resource access
    ?_assertEqual({ok,content,text_resource(128)}, coap_client:request(get, "coap://127.0.0.1/text/128")),
    ?_assertEqual({ok,content,text_resource(1024)}, coap_client:request(get, "coap://127.0.0.1/text/1024")),
    ?_assertEqual({ok,content,text_resource(1984)}, coap_client:request(get, "coap://127.0.0.1/text/1984")),
    ?_assertEqual({error,method_not_allowed}, coap_client:request(post, "coap://127.0.0.1/text", text_resource(128))),
    ?_assertEqual({error,method_not_allowed}, coap_client:request(post, "coap://127.0.0.1/text", text_resource(1024))),
    ?_assertEqual({error,method_not_allowed}, coap_client:request(post, "coap://127.0.0.1/text", text_resource(1984))),
    ?_assertEqual({error,method_not_allowed}, coap_client:request(put, "coap://127.0.0.1/text")),
    ?_assertEqual({error,method_not_allowed}, coap_client:request(delete, "coap://127.0.0.1/text"))
    ].

% end of file
