%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(coap_test).
-export([text_resource/1, text_resource/2, observe/1, observe_and_modify/2]).

-include("coap.hrl").

text_resource(Size) ->
    text_resource(undefined, Size).
text_resource(ETag, Size) ->
    #coap_content{etag=ETag, format= <<"text/plain">>, payload=large_binary(Size, <<"X">>)}.

large_binary(Size, Acc) when Size > 2*byte_size(Acc) ->
    large_binary(Size, <<Acc/binary, Acc/binary>>);
large_binary(Size, Acc) ->
    Sup = binary:part(Acc, 0, Size-byte_size(Acc)),
    <<Acc/binary, Sup/binary>>.

observe_and_modify(Uri, Resource) ->
    {ok, _} = timer:apply_after(500, coap_client, request, [put, Uri, Resource]),
    observe(Uri).

observe(Uri) ->
    case coap_observer:observe(Uri) of
        {ok, Pid, N1, Code1, Content1} ->
            % wait for one notification and then stop
            receive
                {coap_notify, Pid, N2, Code2, Content2} ->
                    {{ok, pid, N1, Code1, Content1},            % answer to the observe request
                     {coap_notify, pid, N2, Code2, Content2},   % notification
                     coap_observer:stop(Pid)}                   % answer to the cancellation
            end;
        NotSubscribed ->
            NotSubscribed
    end.

% end of file
