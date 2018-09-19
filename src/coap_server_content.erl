%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% provides the .well-known/core resource and content queries
-module(coap_server_content).
-behaviour(coap_resource).

-export([coap_discover/2, coap_get/5, coap_post/4, coap_put/4, coap_delete/3,
    coap_observe/5, coap_unobserve/1, handle_info/2, coap_ack/2]).

-include("coap.hrl").

coap_discover(_Prefix, _Args) ->
    [].

coap_get(_ChId, _Prefix, [], Query, _Content) ->
    Links = core_link:encode(filter(coap_server_registry:get_links(), Query)),
    #coap_content{etag = binary:part(crypto:hash(sha, Links), {0,4}),
                  format = <<"application/link-format">>,
                  payload = list_to_binary(Links)};
coap_get(_ChId, _Prefix, _Else, _Query, _Content) ->
    {error, not_found}.

coap_post(_ChId, _Prefix, _Suffix, _Content) -> {error, method_not_allowed}.
coap_put(_ChId, _Prefix, _Suffix, _Content) -> {error, method_not_allowed}.
coap_delete(_ChId, _Prefix, _Suffix) -> {error, method_not_allowed}.

coap_observe(_ChId, _Prefix, _Suffix, _Ack, _Content) -> {error, method_not_allowed}.
coap_unobserve(_State) -> ok.
handle_info(_Message, State) -> {noreply, State}.
coap_ack(_Ref, State) -> {ok, State}.

% uri-query processing

filter(Links, []) ->
    Links;
filter(Links, [Search | Query]) ->
    filter(
        case binary:split(Search, <<$=>>) of
            [Name0, Value0] ->
                Name = list_to_atom(binary_to_list(Name0)),
                Value = wildcard_value(Value0),
                lists:filter(
                    fun (Link) -> match_link(Link, Name, Value) end,
                    Links);
            _Else ->
                Links
        end,
        Query).

wildcard_value(<<>>) ->
    {global, <<>>};
wildcard_value(Value) ->
    case binary:last(Value) of
        $* -> {prefix, binary:part(Value, 0, byte_size(Value)-1)};
        _Else -> {global, Value}
    end.

match_link({_Type, _Uri, Attrs}, Name, Value0) ->
    lists:any(
        fun (AttrVal) ->
            case Value0 of
                {prefix, Value} -> binary:part(AttrVal, 0, byte_size(Value)) =:= Value;
                {global, Value} -> AttrVal =:= Value
            end
        end,
        proplists:get_all_values(Name, Attrs)).

% end of file
