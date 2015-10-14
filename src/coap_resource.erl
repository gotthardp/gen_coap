%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(coap_resource).

-include("coap.hrl").

-callback coap_get({inet:port_number(), inet:ip_address()},
    [binary()], [binary()]) -> coap_content() | {'error', atom()}.

% end of file
