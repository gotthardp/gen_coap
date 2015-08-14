%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

Definitions.

ALPHA = [a-zA-Z]
DIGIT = [0-9]

PCHAR = ({ALPHA}|{DIGIT}|[-._~])

Rules.

<  : {token, {'<', TokenLine}}.
/  : {token, {'/', TokenLine}}.
>  : {token, {'>', TokenLine}}.
;  : {token, {';', TokenLine}}.
,  : {token, {',', TokenLine}}.
=  : {token, {'=', TokenLine}}.

{PCHAR}+  : {token, {segment, TokenLine, TokenChars}}.
"[^"]*"   : {token, {string, TokenLine, string:strip(TokenChars, both, $\")}}.

Erlang code.
