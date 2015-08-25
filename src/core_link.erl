%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% encoding and decoding for the CoRE link format, see RFC 6690
-module(core_link).

-export([decode/1, encode/1]).

decode(String) ->
    % the parser is auto-generated using leex and yecc
    case catch core_link_scanner:string(String) of
        {ok, TokenList, _Line} ->
            case catch core_link_parser:parse(TokenList) of
                {ok, Res} -> Res;
                Err -> {error, Err}
            end;
        Err -> {error, Err}
    end.

encode(LinkList) ->
    lists:foldl(
        fun (Link, []) -> encode_link_value(Link);
            (Link, Str) -> Str++","++encode_link_value(Link)
        end, [], LinkList).

encode_link_value({UriType, UriList, Attrs}) ->
    encode_link_uri(UriType, UriList)++encode_link_params(Attrs).

encode_link_params(Attrs) ->
    lists:foldl(
        fun(Attr, Acc) -> Acc ++ encode_link_param(Attr)
        end, [], Attrs).

encode_link_uri(absolute, UriList) -> "</"++string:join(UriList, "/")++">";
encode_link_uri(rootless, UriList) -> "<"++string:join(UriList, "/")++">".

encode_link_param({rt, Value}) -> ";rt=\"" ++ Value ++ "\"";
encode_link_param({Other, Value}) -> ";"++atom_to_list(Other)++"=\"" ++ Value ++ "\"".


-include_lib("eunit/include/eunit.hrl").

codec_test_() -> [
    test_decode("<link>", [{rootless, ["link"], []}]),
    test_decode("</link1>;par=\"val\",<link2>;par=\"val\";par2=\"val2\"",
        [{absolute, ["link1"], [{par, "val"}]}, {rootless, ["link2"], [{par, "val"}, {par2, "val2"}]}]),
    test_decode("/link", error)].

test_decode(String, error) ->
    Struct2 = decode(String),
    ?_assertMatch({error, _}, Struct2);

test_decode(String, Struct) ->
    Struct2 = decode(String),
    [?_assertEqual(Struct, Struct2),
    % try reverse encoding of the decoded structure
    test_encode(Struct2, String)].

test_encode(Struct, String) ->
    String2 = encode(Struct),
    ?_assertEqual(String, String2).

% end of file
