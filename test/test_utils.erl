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

% end of file
