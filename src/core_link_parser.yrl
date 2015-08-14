%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

Nonterminals LINKLIST LINK URI PARAMS PARAM.
Terminals '<' '/' '>' ';' ',' '=' segment string.
Rootsymbol LINKLIST.

LINKLIST -> LINK : ['$1'].
LINKLIST -> LINK ',' LINKLIST : ['$1'|'$3'].

LINK -> '<' '/' URI '>' PARAMS : {absolute, '$3', '$5'}.
LINK -> '<' URI '>' PARAMS : {rootless, '$2', '$4'}.

URI  -> segment : [strval('$1')].
URI  -> segment '/' URI : [strval('$1')|'$3'].

PARAMS -> ';' PARAM PARAMS: ['$2'|'$3'].
PARAMS -> '$empty' : [].

PARAM -> segment '=' segment : {atomval('$1'), strval('$3')}.
PARAM -> segment '=' string : {atomval('$1'), strval('$3')}.

Erlang code.

strval({_, _, Val}) -> Val.
atomval({_, _, Val}) -> list_to_atom(Val).
