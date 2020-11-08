%%%-------------------------------------------------------------------
%%% @author benoitc
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Aug 2018 14:36
%%%-------------------------------------------------------------------
-module(uts46_emoji_test).
-author("benoitc").

%% API
-export([uts46_encode_test/0, uts46_decode_test/0]).


-include_lib("eunit/include/eunit.hrl").

uts46_encode_test() ->
  ?assertEqual("xn--mp8hai.fm1", idna:encode("👁👄👁.fm", [uts46])).

uts46_decode_test() ->
  ?assertEqual("👁👄👁.fm", idna:decode("xn--mp8hai.fm", [uts46])).
