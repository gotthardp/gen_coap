%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

Definitions.

ALPHA = [a-zA-Z]
DIGIT = [0-9]
HEXDIG = ({DIGIT}|[a-fA-F])

PCT = %{HEXDIG}{HEXDIG}
PCHAR = ({ALPHA}|{DIGIT}|{PCT}|[-._~])

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
