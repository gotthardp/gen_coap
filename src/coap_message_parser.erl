%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% implements CoAP version 1 message encoding and decoding
-module(coap_message_parser).

-export([decode/1, decode_type/1, encode/1]).
-include("coap.hrl").

-define(VERSION, 1).

-define(OPTION_OBSERVE, 6). % draft-ietf-core-observe-16
-define(OPTION_URI_PATH, 11).
-define(OPTION_CONTENT_FORMAT, 12).
-define(OPTION_ACCEPT, 17).

% empty message
decode(<<?VERSION:2, Type:2, 0:4, 0:3, 0:5, MsgId:16, Tail/bytes>>) ->
    {Options, <<>>} = decode_option_list(Tail),
    #coap_message{
        type=decode_type(Type),
        id=MsgId,
        options=Options};
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
encode(#coap_message{type=Type, method=undefined, id=MsgId, options=Options}) ->
    Tail = encode_option_list(Options, <<>>),
    <<?VERSION:2, (encode_type(Type)):2, 0:4, 0:3, 0:5, MsgId:16, Tail/bytes>>;
encode(#coap_message{type=Type, method=Method, id=MsgId, token=Token, options=Options, payload=Payload}) ->
    TKL = byte_size(Token),
    {Class, Code} = encode_enum(methods(), Method),
    Tail = encode_option_list(Options, Payload),
    <<?VERSION:2, (encode_type(Type)):2, TKL:4, Class:3, Code:5, MsgId:16, Token:TKL/bytes, Tail/bytes>>.

decode_type(0) -> con;
decode_type(1) -> non;
decode_type(2) -> ack;
decode_type(3) -> reset.

encode_type(con) -> 0;
encode_type(non) -> 1;
encode_type(ack) -> 2;
encode_type(reset) -> 3.

methods() ->
% RFC 7252
    [{{0,01}, 'get'},
    {{0,02}, post},
    {{0,03}, 'put'},
    {{0,04}, delete},
    {{2,01}, created},
    {{2,02}, deleted},
    {{2,03}, valid},
    {{2,04}, changed},
    {{2,05}, content},
    {{4,00}, bad_request},
    {{4,01}, uauthorized},
    {{4,02}, bad_option},
    {{4,03}, forbidden},
    {{4,04}, not_found},
    {{4,05}, method_not_allowed},
    {{4,06}, not_acceptable},
    {{4,12}, precondition_failed},
    {{4,13}, request_entity_too_large},
    {{4,15}, unsupported_content_format},
    {{5,00}, internal_server_error},
    {{5,01}, not_implemented},
    {{5,02}, bad_gateway},
    {{5,03}, service_unavailable},
    {{5,04}, gateway_timeout},
    {{5,05}, proxying_not_supported}].

content_formats() ->
    [{0, <<"text/plain">>},
    {40, <<"application/link-format">>},
    {41, <<"application/xml">>},
    {42, <<"application/octet-stream">>},
    {47, <<"application/exi">>},
    {50, <<"application/json">>}].

decode_enum(Dict, Value) ->
    decode_enum(Dict, Value, undefined).
decode_enum(Dict, Value, Default) ->
    case [Y || {X, Y} <- Dict, X =:= Value] of
        [Res] -> Res;
        [] -> Default
    end.

encode_enum(Dict, Value) ->
    encode_enum(Dict, Value, undefined).
encode_enum(Dict, Value, Default) ->
    case [X || {X, Y} <- Dict, Y =:= Value] of
        [Res] -> Res;
        [] -> Default
    end.

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
append_option({SameOptId, OptVal2}, [{SameOptId, OptVal1} | OptionList]) -> [{SameOptId, OptVal1++[OptVal2]} | OptionList];
append_option({OptId2, OptVal2}, OptionList) -> [{OptId2, [OptVal2]} | OptionList].

encode_option_list(Options, <<>>) ->
    encode_option_list1(Options);
encode_option_list(Options, Payload) ->
    <<(encode_option_list1(Options))/bytes, 16#FF, Payload/bytes>>.

encode_option_list1(Options) ->
    Options1 = lists:foldl(
        fun(Option, Acc) -> split_and_encode_option(Option)++Acc end,
        [], Options),
    % sort before encoding so we can calculate the deltas
    % the sort is stable; it maintains relative order of values with equal keys
    encode_option_list(lists:keysort(1, Options1), 0, <<>>).

split_and_encode_option({OptId, [OptVal1 | OptVals]}) ->
    [encode_option({OptId, OptVal1}) | split_and_encode_option({OptId, OptVals})];
split_and_encode_option({_OptId, []}) ->
    [].

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

% RFC 7252
decode_option(?OPTION_URI_PATH, OptVal) ->
    {uri_path, binary_to_list(OptVal)};
decode_option(?OPTION_CONTENT_FORMAT, OptVal) ->
    Num = binary:decode_unsigned(OptVal),
    {content_format, decode_enum(content_formats(), Num, Num)};
decode_option(?OPTION_ACCEPT, OptVal) ->
    {'accept', binary:decode_unsigned(OptVal)};
% draft-ietf-core-observe-16
decode_option(?OPTION_OBSERVE, OptVal) ->
    {observe, binary:decode_unsigned(OptVal)};
% unknown option
decode_option(OptNum, OptVal) ->
    {OptNum, OptVal}.

% RFC 7252
encode_option({uri_path, OptVal}) ->
    {?OPTION_URI_PATH, list_to_binary(OptVal)};
encode_option({content_format, OptVal}) when is_integer(OptVal) ->
    {?OPTION_CONTENT_FORMAT, binary:encode_unsigned(OptVal)};
encode_option({content_format, OptVal}) ->
    Num = encode_enum(content_formats(), OptVal),
    {?OPTION_CONTENT_FORMAT, binary:encode_unsigned(Num)};
encode_option({'accept', OptVal}) ->
    {?OPTION_ACCEPT, binary:encode_unsigned(OptVal)};
% draft-ietf-core-observe-16
encode_option({observe, OptVal}) ->
    {?OPTION_OBSERVE, binary:encode_unsigned(OptVal)};
% unknown option
encode_option({OptNum, OptVal}) when is_integer(OptNum) ->
    {OptNum, OptVal}.


-include_lib("eunit/include/eunit.hrl").

codec_test_()-> [
    test_codec(#coap_message{type=reset, id=0, options=[]}),
    test_codec(#coap_message{type=con, method='get', id=100,
        options=[{observe, [1]}]}),
    test_codec(#coap_message{type=non, method='put', id=200, token= <<"token">>,
        options=[{uri_path,[".well-known", "core"]}]}),
    test_codec(#coap_message{type=non, method='content', id=200, token= <<"token">>,
        payload= <<"<url>">>, options=[{content_format, [<<"application/link-format">>]}, {uri_path,[".well-known", "core"]}]})].

test_codec(Message) ->
    Message2 = encode(Message),
    Message1 = decode(Message2),
    ?_assertEqual(Message, Message1).

% end of file
