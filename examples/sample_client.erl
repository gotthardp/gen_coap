-module(sample_client).
-export([start/1]).

-include("coap.hrl").

-record(command, {method=get, uri, observe, content=#coap_content{}}).

parse_cmdline(["-m", Method | Params], Command) ->
    parse_cmdline(Params, Command#command{method=list_to_atom(Method)});
parse_cmdline(["-e", Text | Params], Command=#command{content=Content}) ->
    parse_cmdline(Params, Command#command{content=Content#coap_content{payload=list_to_binary(Text)}});
parse_cmdline(["-s", Observe | Params], Command) ->
    parse_cmdline(Params, Command#command{observe=list_to_integer(Observe)});
parse_cmdline([[$- | _] | _Params], _Command) ->
    throw({error, "Unrecognized option"});
parse_cmdline([Uri | Params], Command) ->
    parse_cmdline(Params, Command#command{uri=Uri});
parse_cmdline([], Command) ->
    Command.

start(Params) ->
    case catch parse_cmdline(Params, #command{}) of
        {command, Method, Uri, undefined, Content} -> request(Method, Uri, Content);
        {command, get, Uri, Duration, _Content} -> observe(Uri, Duration);
        {error, Error} -> io:format("~p~n", [Error])
    end.

request(Method, Uri, Content) ->
    io:format("~p ~p~n", [Method, Uri]),
    Res = coap_client:request(Method, Uri, Content),
    io:format("~p~n", [Res]).

observe(Uri, Duration) ->
    io:format("observe ~p~n", [Uri]),
    case coap_observer:observe(Uri) of
        {ok, Pid, N, Code, Content} ->
            io:format("~p ~p ~p~n", [N, Code, Content]),
            erlang:send_after(Duration*1000, self(), stop),
            wait_for_notify(Pid);
        NotSubscribed ->
            io:format("~p~n", [NotSubscribed])
    end.

wait_for_notify(Pid) ->
    receive
        % observation continues
        {coap_notify, Pid, N, Code, Content} when is_integer(N) ->
            io:format("~p ~p ~p~n", [N, Code, Content]),
            wait_for_notify(Pid);
        % observation was cancelled
        {coap_notify, Pid, undefined, Code, Content} ->
            io:format("~p ~p~n", [Code, Content]),
            ok;
        stop ->
            coap_observer:stop(Pid),
            ok
    end.

% end of file
