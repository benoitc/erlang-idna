%% -*- coding: utf-8 -*-
%%%
%%% This file is part of erlang-idna released under the MIT license.
%%% See the LICENSE for more information.
%%%
%% @doc Unicode Normalization Form C and lowercase mapping.
%%
%% Uses the tables generated into `idna_data' from the same Unicode
%% version as the IDNA tables, so results do not depend on the Unicode
%% version of the OTP release (UAX #15, Unicode Standard Section 3.11).
-module(idna_nfc).

-export([nfc/1, lowercase/1]).

-define(S_BASE, 16#AC00).
-define(L_BASE, 16#1100).
-define(V_BASE, 16#1161).
-define(T_BASE, 16#11A7).
-define(L_COUNT, 19).
-define(V_COUNT, 21).
-define(T_COUNT, 28).
-define(N_COUNT, (?V_COUNT * ?T_COUNT)).
-define(S_COUNT, (?L_COUNT * ?N_COUNT)).

%% @doc Normalize a list of code points to NFC.
-spec nfc([non_neg_integer()]) -> [non_neg_integer()].
nfc(CPs) ->
  compose(reorder(decompose(CPs))).

%% @doc Map each code point to its full lowercase mapping.
-spec lowercase([non_neg_integer()]) -> [non_neg_integer()].
lowercase(CPs) ->
  lists:flatmap(fun(CP) when CP < 128 ->
                      if CP >= $A, CP =< $Z -> [CP + 32];
                         true -> [CP]
                      end;
                   (CP) -> idna_data:lowercase(CP)
                end, CPs).

%% Canonical decomposition, Hangul syllables included.
decompose(CPs) ->
  lists:flatmap(fun decompose_cp/1, CPs).

decompose_cp(CP) when CP < 16#C0 -> [CP];
decompose_cp(CP) when CP >= ?S_BASE, CP < ?S_BASE + ?S_COUNT ->
  SIndex = CP - ?S_BASE,
  L = ?L_BASE + SIndex div ?N_COUNT,
  V = ?V_BASE + (SIndex rem ?N_COUNT) div ?T_COUNT,
  case SIndex rem ?T_COUNT of
    0 -> [L, V];
    T -> [L, V, ?T_BASE + T]
  end;
decompose_cp(CP) ->
  case idna_data:decomposition(CP) of
    [] -> [CP];
    D -> D
  end.

%% Canonical ordering: sort each run of non-starters by combining class,
%% keeping the original order of equal classes (lists:keysort is stable).
reorder(CPs) ->
  reorder(CPs, []).

reorder([], Acc) ->
  lists:reverse(Acc);
reorder([CP | Rest] = CPs, Acc) ->
  case idna_data:ccc(CP) of
    0 -> reorder(Rest, [CP | Acc]);
    _ ->
      {Run, Rest1} = lists:splitwith(fun(C) -> idna_data:ccc(C) =/= 0 end, CPs),
      Sorted = [C || {_, C} <- lists:keysort(1, [{idna_data:ccc(C), C} || C <- Run])],
      reorder(Rest1, lists:reverse(Sorted, Acc))
  end.

%% Canonical composition. Walks the decomposed string keeping the last
%% starter and the code points after it; a code point combines with the
%% starter unless a code point with a combining class greater than or
%% equal to its own sits in between (it is then blocked).
compose([]) ->
  [];
compose([First | Rest]) ->
  compose(Rest, First, idna_data:ccc(First) =:= 0, [], -1, []).

%% compose(Input, Starter, StarterIsStarter, PendingRev, LastCCC, AccRev)
compose([], Starter, _, Pending, _, Acc) ->
  lists:reverse(Acc, [Starter | lists:reverse(Pending)]);
compose([CP | Rest], Starter, true, Pending, LastCCC, Acc) ->
  CCC = idna_data:ccc(CP),
  Blocked = Pending =/= [] andalso (LastCCC =:= 0 orelse LastCCC >= CCC),
  case (not Blocked) andalso combine(Starter, CP) of
    Composite when is_integer(Composite) ->
      compose(Rest, Composite, true, Pending, LastCCC, Acc);
    _ when CCC =:= 0 ->
      compose(Rest, CP, true, [], -1, Pending ++ [Starter | Acc]);
    _ ->
      compose(Rest, Starter, true, [CP | Pending], CCC, Acc)
  end;
compose([CP | Rest], Starter, false, Pending, _LastCCC, Acc) ->
  %% No starter yet: emit code points until one shows up.
  compose(Rest, CP, idna_data:ccc(CP) =:= 0, [], -1, Pending ++ [Starter | Acc]).

combine(L, V) when L >= ?L_BASE, L < ?L_BASE + ?L_COUNT, V >= ?V_BASE, V < ?V_BASE + ?V_COUNT ->
  ?S_BASE + ((L - ?L_BASE) * ?V_COUNT + (V - ?V_BASE)) * ?T_COUNT;
combine(LV, T) when LV >= ?S_BASE, LV < ?S_BASE + ?S_COUNT, (LV - ?S_BASE) rem ?T_COUNT =:= 0,
                    T > ?T_BASE, T < ?T_BASE + ?T_COUNT ->
  LV + (T - ?T_BASE);
combine(A, B) ->
  idna_data:compose(A, B).
