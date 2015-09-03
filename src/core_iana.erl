%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% values assigned by IANA
% http://www.iana.org/assignments/core-parameters
-module(core_iana).

-export([content_formats/0]).
-export([decode_enum/2, decode_enum/3, encode_enum/2, encode_enum/3]).

content_formats() ->
    [{0, <<"text/plain">>},
    {40, <<"application/link-format">>},
    {41, <<"application/xml">>},
    {42, <<"application/octet-stream">>},
    {47, <<"application/exi">>},
    {50, <<"application/json">>},
    {60, <<"application/cbor">>}].


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

% end of file
