%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% encoding and decoding for CoAP v1 messages
-module(coap_message_parser).

-export([decode/1, decode_type/1, encode/1, message_id/1]).
-import(core_iana, [content_formats/0]).
-import(core_iana, [decode_enum/2, decode_enum/3, encode_enum/2, encode_enum/3]).
-include("coap.hrl").

-define(VERSION, 1).

-define(OPTION_IF_MATCH, 1).
-define(OPTION_URI_HOST, 3).
-define(OPTION_ETAG, 4).
-define(OPTION_IF_NONE_MATCH, 5).
-define(OPTION_OBSERVE, 6). % draft-ietf-core-observe-16
-define(OPTION_URI_PORT, 7).
-define(OPTION_LOCATION_PATH, 8).
-define(OPTION_URI_PATH, 11).
-define(OPTION_CONTENT_FORMAT, 12).
-define(OPTION_MAX_AGE, 14).
-define(OPTION_URI_QUERY, 15).
-define(OPTION_ACCEPT, 17).
-define(OPTION_LOCATION_QUERY, 20).
-define(OPTION_BLOCK2, 23). % draft-ietf-core-block-17
-define(OPTION_BLOCK1, 27).
-define(OPTION_PROXY_URI, 35).
-define(OPTION_PROXY_SCHEME, 39).
-define(OPTION_SIZE1, 60).

% empty message only contains the 4-byte header
decode(<<?VERSION:2, Type:2, 0:4, 0:3, 0:5, MsgId:16>>) ->
    #coap_message{
        type=decode_type(Type),
        id=MsgId};
decode(<<?VERSION:2, Type:2, TKL:4, Class:3, Code:5, MsgId:16, Token:TKL/bytes, Tail/bytes>>) ->
    {Options, Payload} = decode_option_list(Tail),
    #coap_message{
        type=decode_type(Type),
        method=decode_enum(methods(), {Class, Code}),
        id=MsgId,
        token=Token,
        options=Options,
        payload=Payload}.

% empty message
encode(#coap_message{type=Type, method=undefined, id=MsgId}) ->
    <<?VERSION:2, (encode_type(Type)):2, 0:4, 0:3, 0:5, MsgId:16>>;
encode(#coap_message{type=Type, method=Method, id=MsgId, token=Token, options=Options, payload=Payload}) ->
    TKL = byte_size(Token),
    {Class, Code} = encode_enum(methods(), Method),
    Tail = encode_option_list(Options, Payload),
    <<?VERSION:2, (encode_type(Type)):2, TKL:4, Class:3, Code:5, MsgId:16, Token:TKL/bytes, Tail/bytes>>.

% shortcut function for reset generation
message_id(<<_:16, MsgId:16, _Tail/bytes>>) -> MsgId;
message_id(#coap_message{id=MsgId}) -> MsgId.

decode_type(0) -> con;
decode_type(1) -> non;
decode_type(2) -> ack;
decode_type(3) -> reset.

encode_type(con) -> 0;
encode_type(non) -> 1;
encode_type(ack) -> 2;
encode_type(reset) -> 3.

methods() -> [
% RFC 7252
    % atom indicate a request
    {{0,01}, get},
    {{0,02}, post},
    {{0,03}, put},
    {{0,04}, delete},
    % success is a tuple {ok, ...}
    {{2,01}, {ok, created}},
    {{2,02}, {ok, deleted}},
    {{2,03}, {ok, valid}},
    {{2,04}, {ok, changed}},
    {{2,05}, {ok, content}},
    {{2,07}, {ok, nocontent}},
    {{2,31}, {ok, continue}}, % block
    % error is a tuple {error, ...}
    {{4,00}, {error, bad_request}},
    {{4,01}, {error, unauthorized}},
    {{4,02}, {error, bad_option}},
    {{4,03}, {error, forbidden}},
    {{4,04}, {error, not_found}},
    {{4,05}, {error, method_not_allowed}},
    {{4,06}, {error, not_acceptable}},
    {{4,08}, {error, request_entity_incomplete}}, % block
    {{4,12}, {error, precondition_failed}},
    {{4,13}, {error, request_entity_too_large}},
    {{4,15}, {error, unsupported_content_format}},
    {{5,00}, {error, internal_server_error}},
    {{5,01}, {error, not_implemented}},
    {{5,02}, {error, bad_gateway}},
    {{5,03}, {error, service_unavailable}},
    {{5,04}, {error, gateway_timeout}},
    {{5,05}, {error, proxying_not_supported}}].

% option parsing is based on Patrick's CoAP Message Parsing in Erlang
% https://gist.github.com/azdle/b2d477ff183b8bbb0aa0

decode_option_list(Tail) ->
    decode_option_list(Tail, 0, []).

decode_option_list(<<>>, _LastNum, OptionList) ->
    {OptionList, <<>>};
decode_option_list(<<16#FF, Payload/bytes>>, _LastNum, OptionList) ->
    {OptionList, Payload};
decode_option_list(<<Delta:4, Len:4, Tail/bytes>>, LastNum, OptionList) ->
    {Tail1, OptNum} = if
        Delta < 13 ->
            {Tail, LastNum + Delta};
        Delta == 13 ->
            <<ExtOptNum, NewTail1/bytes>> = Tail,
            {NewTail1, LastNum + ExtOptNum + 13};
        Delta == 14 ->
            <<ExtOptNum:16, NewTail1/bytes>> = Tail,
            {NewTail1, LastNum + ExtOptNum + 269}
    end,
    {Tail2, OptLen} = if
        Len < 13 ->
            {Tail1, Len};
        Len == 13 ->
            <<ExtOptLen, NewTail2/bytes>> = Tail1,
            {NewTail2, ExtOptLen + 13};
        Len == 14 ->
            <<ExtOptLen:16, NewTail2/bytes>> = Tail1,
            {NewTail2, ExtOptLen + 269}
    end,
    case Tail2 of
        <<OptVal:OptLen/bytes, NextOpt/bytes>> ->
            decode_option_list(NextOpt, OptNum, append_option(decode_option(OptNum, OptVal), OptionList));
        <<>> ->
            decode_option_list(<<>>, OptNum, append_option(decode_option(OptNum, <<>>), OptionList))
    end.

% put options of the same id into one list
append_option({SameOptId, OptVal2}, [{SameOptId, OptVal1} | OptionList]) ->
    case is_repeatable_option(SameOptId) of
        true ->
            % we must keep the order
            [{SameOptId, OptVal1++[OptVal2]} | OptionList];
        false ->
            throw({error, atom_to_list(SameOptId)++" is not repeatable"})
    end;
append_option({OptId2, OptVal2}, OptionList) ->
    case is_repeatable_option(OptId2) of
        true -> [{OptId2, [OptVal2]} | OptionList];
        false -> [{OptId2, OptVal2} | OptionList]
    end.

encode_option_list(Options, <<>>) ->
    encode_option_list1(Options);
encode_option_list(Options, Payload) ->
    <<(encode_option_list1(Options))/bytes, 16#FF, Payload/bytes>>.

encode_option_list1(Options) ->
    Options1 = encode_options(Options, []),
    % sort before encoding so we can calculate the deltas
    % the sort is stable; it maintains relative order of values with equal keys
    encode_option_list(lists:keysort(1, Options1), 0, <<>>).

encode_options([{_OptId, undefined} | OptionList], Acc) ->
    encode_options(OptionList, Acc);
encode_options([{OptId, OptVal} | OptionList], Acc) ->
    case is_repeatable_option(OptId) of
        true ->
            encode_options(OptionList, split_and_encode_option({OptId, OptVal}, Acc));
        false ->
            encode_options(OptionList, [encode_option({OptId, OptVal}) | Acc])
    end;
encode_options([], Acc) ->
    Acc.

split_and_encode_option({OptId, [undefined | OptVals]}, Acc) ->
    split_and_encode_option({OptId, OptVals}, Acc);
split_and_encode_option({OptId, [OptVal1 | OptVals]}, Acc) ->
    % we must keep the order
    [encode_option({OptId, OptVal1}) | split_and_encode_option({OptId, OptVals}, Acc)];
split_and_encode_option({_OptId, []}, Acc) ->
    Acc.

encode_option_list([{OptNum, OptVal} | OptionList], LastNum, Acc) ->
    {Delta, ExtNum} = if
        OptNum - LastNum >= 269 ->
            {14, <<(OptNum - LastNum - 269):16>>};
        OptNum - LastNum >= 13 ->
            {13, <<(OptNum - LastNum - 13)>>};
        true ->
            {OptNum - LastNum, <<>>}
    end,
    {Len, ExtLen} = if
        byte_size(OptVal) >= 269 ->
            {14, <<(byte_size(OptVal) - 269):16>>};
        byte_size(OptVal) >= 13 ->
            {13, <<(byte_size(OptVal) - 13)>>};
        true ->
            {byte_size(OptVal), <<>>}
    end,
    Acc2 = <<Acc/bytes, Delta:4, Len:4, ExtNum/bytes, ExtLen/bytes, OptVal/bytes>>,
    encode_option_list(OptionList, OptNum, Acc2);

encode_option_list([], _LastNum, Acc) ->
    Acc.

is_repeatable_option(if_match) -> true;
is_repeatable_option(etag) -> true;
is_repeatable_option(location_path) -> true;
is_repeatable_option(uri_path) -> true;
is_repeatable_option(uri_query) -> true;
is_repeatable_option(location_query) -> true;
is_repeatable_option(_Else) -> false.

% RFC 7252
decode_option(?OPTION_IF_MATCH, OptVal) -> {if_match, OptVal};
decode_option(?OPTION_URI_HOST, OptVal) -> {uri_host, OptVal};
decode_option(?OPTION_ETAG, OptVal) -> {etag, OptVal};
decode_option(?OPTION_IF_NONE_MATCH, <<>>) -> {if_none_match, true};
decode_option(?OPTION_URI_PORT, OptVal) -> {uri_port, binary:decode_unsigned(OptVal)};
decode_option(?OPTION_LOCATION_PATH, OptVal) -> {location_path, OptVal};
decode_option(?OPTION_URI_PATH, OptVal) -> {uri_path, OptVal};
decode_option(?OPTION_CONTENT_FORMAT, OptVal) ->
    Num = binary:decode_unsigned(OptVal),
    {content_format, decode_enum(content_formats(), Num, Num)};
decode_option(?OPTION_MAX_AGE, OptVal) -> {max_age, binary:decode_unsigned(OptVal)};
decode_option(?OPTION_URI_QUERY, OptVal) -> {uri_query, OptVal};
decode_option(?OPTION_ACCEPT, OptVal) -> {'accept', binary:decode_unsigned(OptVal)};
decode_option(?OPTION_LOCATION_QUERY, OptVal) -> {location_query, OptVal};
decode_option(?OPTION_PROXY_URI, OptVal) -> {proxy_uri, OptVal};
decode_option(?OPTION_PROXY_SCHEME, OptVal) -> {proxy_scheme, OptVal};
decode_option(?OPTION_SIZE1, OptVal) -> {size1, binary:decode_unsigned(OptVal)};
% draft-ietf-core-observe-16
decode_option(?OPTION_OBSERVE, OptVal) -> {observe, binary:decode_unsigned(OptVal)};
% draft-ietf-core-block-17
decode_option(?OPTION_BLOCK2, OptVal) -> {block2, decode_block(OptVal)};
decode_option(?OPTION_BLOCK1, OptVal) -> {block1, decode_block(OptVal)};
% unknown option
decode_option(OptNum, OptVal) -> {OptNum, OptVal}.

decode_block(<<Num:4, M:1, SizEx:3>>) -> decode_block1(Num, M, SizEx);
decode_block(<<Num:12, M:1, SizEx:3>>) -> decode_block1(Num, M, SizEx);
decode_block(<<Num:28, M:1, SizEx:3>>) -> decode_block1(Num, M, SizEx).

decode_block1(Num, M, SizEx) ->
    {Num, if M == 0 -> false; true -> true end, trunc(math:pow(2, SizEx+4))}.


% RFC 7252
encode_option({if_match, OptVal}) -> {?OPTION_IF_MATCH, OptVal};
encode_option({uri_host, OptVal}) -> {?OPTION_URI_HOST, OptVal};
encode_option({etag, OptVal}) -> {?OPTION_ETAG, OptVal};
encode_option({if_none_match, true}) -> {?OPTION_IF_NONE_MATCH, <<>>};
encode_option({uri_port, OptVal}) -> {?OPTION_URI_PORT, binary:encode_unsigned(OptVal)};
encode_option({location_path, OptVal}) -> {?OPTION_LOCATION_PATH, OptVal};
encode_option({uri_path, OptVal}) -> {?OPTION_URI_PATH, OptVal};
encode_option({content_format, OptVal}) when is_integer(OptVal) ->
    {?OPTION_CONTENT_FORMAT, binary:encode_unsigned(OptVal)};
encode_option({content_format, OptVal}) ->
    Num = encode_enum(content_formats(), OptVal),
    {?OPTION_CONTENT_FORMAT, binary:encode_unsigned(Num)};
encode_option({max_age, OptVal}) -> {?OPTION_MAX_AGE, binary:encode_unsigned(OptVal)};
encode_option({uri_query, OptVal}) -> {?OPTION_URI_QUERY, OptVal};
encode_option({'accept', OptVal}) -> {?OPTION_ACCEPT, binary:encode_unsigned(OptVal)};
encode_option({location_query, OptVal}) -> {?OPTION_LOCATION_QUERY, OptVal};
encode_option({proxy_uri, OptVal}) -> {?OPTION_PROXY_URI, OptVal};
encode_option({proxy_scheme, OptVal}) -> {?OPTION_PROXY_SCHEME, OptVal};
encode_option({size1, OptVal}) -> {?OPTION_SIZE1, binary:encode_unsigned(OptVal)};
% draft-ietf-core-observe-16
encode_option({observe, OptVal}) -> {?OPTION_OBSERVE, binary:encode_unsigned(OptVal)};
% draft-ietf-core-block-17
encode_option({block2, OptVal}) -> {?OPTION_BLOCK2, encode_block(OptVal)};
encode_option({block1, OptVal}) -> {?OPTION_BLOCK1, encode_block(OptVal)};
% unknown option
encode_option({OptNum, OptVal}) when is_integer(OptNum) ->
    {OptNum, OptVal}.

encode_block({Num, More, Size}) ->
    encode_block1(Num, if More -> 1; true -> 0 end, trunc(log2(Size))-4).

encode_block1(Num, M, SizEx) when Num < 16 ->
    <<Num:4, M:1, SizEx:3>>;
encode_block1(Num, M, SizEx) when Num < 4096 ->
    <<Num:12, M:1, SizEx:3>>;
encode_block1(Num, M, SizEx) ->
    <<Num:28, M:1, SizEx:3>>.

% log2 is not available in R16B
log2(X) -> math:log(X) / math:log(2).

-include_lib("eunit/include/eunit.hrl").

% note that the options below must be sorted by the option numbers
codec_test_()-> [
    test_codec(#coap_message{type=reset, id=0, options=[]}),
    test_codec(#coap_message{type=con, method=get, id=100,
        options=[{block1, {0,true,128}}, {observe, 1}]}),
    test_codec(#coap_message{type=non, method=put, id=200, token= <<"token">>,
        options=[{uri_path,[<<".well-known">>, <<"core">>]}]}),
    test_codec(#coap_message{type=non, method={ok, 'content'}, id=300, token= <<"token">>,
        payload= <<"<url>">>, options=[{content_format, <<"application/link-format">>}, {uri_path,[<<".well-known">>, <<"core">>]}]})].

test_codec(Message) ->
    Message2 = encode(Message),
    Message1 = decode(Message2),
    ?_assertEqual(Message, Message1).

% end of file
