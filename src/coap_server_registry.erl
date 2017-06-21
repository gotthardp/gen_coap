%
% The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Copyright (c) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% registry of server content handlers
-module(coap_server_registry).
-export([add_handler/3, get_handler/1, get_links/0]).

-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {reg}).

add_handler(Prefix, Module, Args) ->
    gen_server:call(?MODULE, {add_handler, Prefix, Module, Args}).

get_handler(Uri) ->
    gen_server:call(?MODULE, {get_handler, Uri}).

get_links() ->
    gen_server:call(?MODULE, {get_links}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{reg=
        % RFC 6690, Section 4
        [{[<<".well-known">>, <<"core">>], coap_server_content, undefined}]
    }}.

handle_call({add_handler, Prefix, Module, Args}, _From, State=#state{reg=Reg}) ->
    NewReg =    case lists:member({Prefix, Module, Args}, Reg) of
                    true  -> Reg;
                    false -> [{Prefix, Module, Args}|Reg]
                end,
    {reply, ok, State#state{reg=NewReg}};


handle_call({get_handler, Uri}, _From, State=#state{reg=Reg}) ->
    {reply, get_handler(Uri, Reg), State};

handle_call({get_links}, _From, State=#state{reg=Reg}) ->
    {reply, lists:usort(get_links(Reg)), State}.


handle_cast(_Unknown, State) ->
    {noreply, State}.

handle_info(_Unknown, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


get_handler(Uri, Reg) ->
    lists:foldl(
        fun(Elem={Prefix, _, _}, Found) ->
            case lists:prefix(Prefix, Uri) of
                true -> one_with_longer_uri(Elem, Found);
                false -> Found
            end
        end,
        undefined, Reg).

% select an entry with a longest prefix
% this allows user to have one handler for "foo" and another for "foo/bar"
one_with_longer_uri(Elem1, undefined) -> Elem1;
one_with_longer_uri(Elem1={Prefix, _, _}, {Match, _, _}) when length(Prefix) > length(Match) -> Elem1;
one_with_longer_uri(_Elem1, Elem2) -> Elem2.

% ask each handler to provide a link list
get_links(Reg) ->
    lists:foldl(
        fun({Prefix, Module, Args}, Acc) -> Acc++get_links(Prefix, Module, Args) end,
        [], Reg).

get_links(Prefix, Module, Args) ->
    case erlang:function_exported(Module, coap_discover, 2) of
        % for each pattern ask the handler to provide a list of resources
        true -> apply(Module, coap_discover, [Prefix, Args]);
        false -> []
    end.


% end of file
