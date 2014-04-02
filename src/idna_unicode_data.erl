%%% -*- erlang -*-
%%%
%%% This file is part of erlang-idna released under the BSD license.
%%% See the LICENSE for more information.
%%%

-module(idna_unicode_data).
-behaviour(gen_server).

%% public api
-export([combining_class/1, compat/1, composition/2, lowercase/1]).
-export([reload/0]).
-export([start_link/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


-include("idna_unicode_data.hrl").


%%============================================================================
%% Constants
%%============================================================================

-define(COMBINING_CLASS, 4).

-define(DECOMPOSITION, 6).

-define(LOWERCASE_MAPPING, 14).

%%============================================================================
%% API
%%============================================================================

combining_class(C) ->
    gen_server:call(?MODULE, {combining_class, C}).

compat(C) ->
    gen_server:call(?MODULE, {compat, C}).

composition(A, B) ->
    gen_server:call(?MODULE, {composition, A, B}).

lowercase(C) ->
    gen_server:call(?MODULE, {lowercase, C}).



reload() ->
    gen_server:call(?MODULE, reload, 10000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%============================================================================
%% gen_server callbacks
%%============================================================================

init(_) ->
    self() ! load,
    {ok, undefined}.

handle_call({combining_class, C}, _From, _State) ->
    case lookup(C) of
        {ok, Props} ->
            {reply, erlang:list_to_integer(element(?COMBINING_CLASS, Props)), _State};
        _ ->
            {reply, 0, _State}
    end;
handle_call({compat, C}, _From, _State) ->
    case process_decomposition(C) of
        undefined ->
            {reply, undefined, _State};
        Value ->
            {reply, Value, _State}
    end;
handle_call({composition, A, B}, _From, _State) ->
    Key = lists:flatten([hex(A), " ", hex(B)]),
    case decomposition(Key) of
        {ok, Props} ->
            {reply, erlang:list_to_integer(element(1, Props), 16), _State};
        {error, bad_codepoint} ->
            {reply, undefined, _State}
    end;
handle_call({lowercase, C}, _From, _State) ->
    Value = process_lowercase(C),
    {reply, Value, _State};
handle_call(reload, _From, _State) ->
    {reply, ok, _State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(load, _State) ->
    {noreply, []};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%============================================================================
%% Helper functions
%%============================================================================

hex(Codepoint) ->
    string:right(erlang:integer_to_list(Codepoint, 16), 4, $0).

dehex(Strings) ->
    [erlang:list_to_integer(String, 16) || String <- Strings].

lookup(Codepoint) ->
    codepoint(Codepoint).

process_decomposition(Codepoint) ->
    case lookup(Codepoint) of
        {ok, Props} ->
            case element(?DECOMPOSITION, Props) of
                [] ->
                    undefined;
                Value ->
                    Tokens = string:tokens(Value, " "),
                    dehex(case hd(Value) of $< -> tl(Tokens); _ -> Tokens end)
            end;
        {error, bad_codepoint} ->
            {error, bad_codepoint}
    end.

process_lowercase(Codepoint) ->
    case lookup(Codepoint) of
        {ok, Props} ->
            case element(?LOWERCASE_MAPPING, Props) of
                [] ->
                    Codepoint;
                Hex ->
                    erlang:list_to_integer(Hex, 16)
            end;
        {error, bad_codepoint} ->
            {error, bad_codepoint}
    end.
