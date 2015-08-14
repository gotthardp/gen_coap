%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% implements the CoRE link format, see RFC 6690
-module(core_link).

-export([decode/1]).

decode(String) ->
    % the parser is auto-generated using leex and yecc
    case catch core_link_scanner:string(String) of
        {ok, TokenList, _Line} ->
            case catch core_link_parser:parse(TokenList) of
                {ok, Res} -> Res;
                _ -> error
            end;
        _ -> error
    end.

-include_lib("eunit/include/eunit.hrl").

codec_test_() -> [
    test_codec("<link>", [{rootless, ["link"], []}]),
    test_codec("</link1>;par=val,<link2>;par=val;par2=val2",
	[{absolute, ["link1"], [{par, "val"}]}, {rootless, ["link2"], [{par, "val"}, {par2, "val2"}]}]),
    test_codec("/link", error)].

test_codec(String, Struct) ->
    ?_assertEqual(Struct, decode(String)).

% end of file
