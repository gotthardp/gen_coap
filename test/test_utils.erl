%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(test_utils).

-export([send_command/1, await_command/2]).
-export([text_resource/1, text_resource/2]).

-include_lib("gen_coap/include/coap.hrl").

send_command(Command) ->
    storage ! {self(), Command},
    receive
        Response -> Response
    end.

await_command(Module, State) ->
    receive
        % command
        {Pid, Command} ->
            case apply(Module, handle, [Command, State]) of
                {Code, State2} ->
                    Pid ! Code,
                    await_command(Module, State2);
                {Code, Data, State2} ->
                    Pid ! {Code, Data},
                    await_command(Module, State2)
            end;
        % termination request
        stop ->
            ok
    end.

text_resource(Size) ->
    text_resource(<<>>, Size).
text_resource(ETag, Size) ->
    #coap_content{etag=ETag, format= <<"text/plain">>, payload=large_binary(Size, <<"X">>)}.

large_binary(Size, Acc) when Size > 2*byte_size(Acc) ->
    large_binary(Size, <<Acc/binary, Acc/binary>>);
large_binary(Size, Acc) ->
    Sup = binary:part(Acc, 0, Size-byte_size(Acc)),
    <<Acc/binary, Sup/binary>>.

% end of file
