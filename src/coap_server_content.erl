%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

% implements registration of server content handlers
% implements the .well-known/code resource and (in future) content queries
-module(coap_server_content).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([add_handler/4, get_handler/1]).

-include("coap.hrl").

-record(state, {reg}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_handler(Process, UriPath, Attrs, Public) ->
    gen_server:call(?MODULE, {add_handler, Process, UriPath, Attrs, Public}).

get_handler(Message) ->
    UriPath = proplists:get_value(uri_path, Message#coap_message.options, []),
    gen_server:call(?MODULE, {get_handler, UriPath}).

init(_Args) ->
    {ok, #state{reg=
        % RFC 6690, Section 4
        [{?MODULE, [".well-known", "core"], [], true}]
    }}.

handle_call({add_handler, Process, UriPath, Attrs, Public}, _From, State=#state{reg=Reg}) ->
    {reply, ok, State#state{reg=[{Process, UriPath, Attrs, Public} | Reg]}};

handle_call({get_handler, UriPath}, _From, State=#state{reg=Reg}) ->
    case lists:foldl(
        fun(Entry={_, RegisteredUri, _, _}, Acc) ->
            case {lists:sublist(UriPath, length(RegisteredUri)), Acc} of
                {RegisteredUri, undefined} -> Entry;
                {RegisteredUri, {BestUri, _}} ->
                    if
                        length(RegisteredUri) > length(BestUri) -> Entry;
                        true -> Acc
                    end;
                _AnythingElse -> Acc
            end
        end, undefined, Reg)
    of
        {Res, _, _, _} -> {reply, Res, State};
        undefined -> {reply, undefined, State}
    end.

handle_cast(Request, State) ->
    io:fwrite("unknown cast ~p~n", [Request]),
    {noreply, State}.

handle_info({coap_request, From, Request=#coap_message{method='get'}}, State=#state{reg=Reg}) ->
    coap_exchange:reply_content(From, Request, <<"application/link-format">>, format_links(Reg)),
    {noreply, State};
handle_info({coap_request, From, Request}, State) ->
    coap_exchange:reply(From, Request, method_not_allowed),
    {noreply, State};
handle_info(_Unknown, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

% encode in the CoRE Link Format (RFC 6690)
format_links(Reg) ->
    Resources = lists:filtermap(
        fun({_, UriList, Attrs, true}) -> {true, encode_link_value(UriList, Attrs)};
            ({_, _, _, false}) -> false
        end, Reg),
    list_to_binary(string:join(Resources, ",")).

encode_link_value(UriList, Attrs) ->
    lists:foldl(
        fun(Attr, Acc) -> Acc ++ encode_link_param(Attr) end,
        encode_link_uri(UriList), Attrs).

encode_link_uri(UriList) -> "</"++string:join(UriList, "/")++">".

encode_link_param({rt, Value}) -> ";rt=\"" ++ Value ++ "\"".

% end of file
