%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% provides the .well-known/core resource and (in future) content queries
-module(coap_server_content).
-behaviour(coap_resource).

-export([coap_discover/2, coap_get/3, coap_post/4, coap_put/4, coap_delete/3,
    coap_observe/4, coap_unobserve/1, handle_info/2, coap_ack/2]).

-include("coap.hrl").

coap_discover(_Prefix, _Args) ->
    [].

coap_get(_ChId, _Prefix, []) ->
    Links = core_link:encode(coap_server_registry:get_links()),
    #coap_content{etag = binary:part(crypto:hash(sha, Links), {0,4}),
                  format = <<"application/link-format">>,
                  payload = list_to_binary(Links)};
coap_get(_ChId, _Prefix, _Else) ->
    {error, not_found}.

coap_post(_ChId, _Prefix, _Suffix, _Content) -> {error, method_not_allowed}.
coap_put(_ChId, _Prefix, _Suffix, _Content) -> {error, method_not_allowed}.
coap_delete(_ChId, _Prefix, _Suffix) -> {error, method_not_allowed}.

coap_observe(_ChId, _Prefix, _Suffix, _Ack) -> {error, method_not_allowed}.
coap_unobserve(_State) -> ok.
handle_info(_Message, State) -> {noreply, State}.
coap_ack(_Ref, State) -> {ok, State}.

% end of file
