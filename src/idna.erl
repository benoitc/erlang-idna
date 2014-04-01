%%% -*- erlang -*-
%%%
%%% This file is part of erlang-idna released under the BSD license.
%%% See the LICENSE for more information.
%%%

-module(idna).
-behaviour(application).

-export([start/0, stop/0]).
-export([to_ascii/1, utf8_to_ascii/1]).
-export([start/2, stop/1]).

-define(ACE_PREFIX, "xn--").


%% helpers

start() -> application:start(idna).
stop() ->  application:stop(idna).

to_ascii(Domain) ->
    case ets_lru:lookup(idna_lru, Domain) of
        not_found ->
            Res = to_ascii(string:tokens(idna_unicode:downcase(Domain), "."),
                           []),
            ets_lru:insert(idna_lru, Domain, Res),
            Res;
        {ok, Res} ->
           Res
    end.

utf8_to_ascii(Domain) ->
    to_ascii(idna_ucs:from_utf8(Domain)).


to_ascii([], Acc) ->
    lists:reverse(Acc);
to_ascii([Label|Labels], []) ->
    to_ascii(Labels, lists:reverse(label_to_ascii(Label)));
to_ascii([Label|Labels], Acc) ->
    to_ascii(Labels, lists:reverse(label_to_ascii(Label), [$.|Acc])).

label_to_ascii(Label) ->
    case lists:all(fun(C) -> idna_ucs:is_ascii(C) end, Label) of
        true ->
            Label;
        false ->
            ?ACE_PREFIX ++ punycode:encode(idna_unicode:normalize_kc(Label))
    end.


%% ============ application API ============

%% @private
%% start the application
start(_StartType, _StartArgs) -> idna_sup:start_link().

%% @private
%% stop the application
stop(_State) -> ok.
