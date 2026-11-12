%% -*- coding: utf-8 -*-
%%%
%%% This file is part of erlang-idna released under the MIT license.
%%% See the LICENSE for more information.
%%%
%% NFC conformance test against NormalizationTest.txt (UAX #15 Section 9).
%%
%% For every row c1..c5: c2 == NFC(c1) == NFC(c2) == NFC(c3) and
%% c4 == NFC(c4) == NFC(c5). Every code point not listed in Part 1 must be
%% unchanged by NFC.
-module(nfc_test).

-include_lib("eunit/include/eunit.hrl").

nfc_conformance_test_() ->
  {timeout, 120, fun conformance/0}.

conformance() ->
  {Rows, Part1} = load(),
  ?assert(length(Rows) > 15000),
  Bad = [R || [C1, C2, C3, C4, C5] = R <- Rows,
              not (idna_nfc:nfc(C1) =:= C2 andalso idna_nfc:nfc(C2) =:= C2
                   andalso idna_nfc:nfc(C3) =:= C2 andalso idna_nfc:nfc(C4) =:= C4
                   andalso idna_nfc:nfc(C5) =:= C4)],
  ?assertEqual([], lists:sublist(Bad, 5)),
  Unlisted = [CP || CP <- lists:seq(0, 16#10FFFF),
                    CP < 16#D800 orelse CP > 16#DFFF,
                    not sets:is_element(CP, Part1),
                    idna_nfc:nfc([CP]) =/= [CP]],
  ?assertEqual([], lists:sublist(Unlisted, 5)).

load() ->
  EbinDir = filename:dirname(code:which(?MODULE)),
  Name = filename:join([filename:dirname(EbinDir), "test", "NormalizationTest.txt"]),
  {ok, Bin} = file:read_file(Name),
  Lines = string:split(binary_to_list(Bin), "\n", all),
  load(Lines, none, [], []).

load([], _Part, Rows, Part1) ->
  {Rows, sets:from_list(Part1)};
load(["@Part" ++ N | Rest], _Part, Rows, Part1) ->
  load(Rest, hd(string:trim(N)), Rows, Part1);
load([[$# | _] | Rest], Part, Rows, Part1) ->
  load(Rest, Part, Rows, Part1);
load(["" | Rest], Part, Rows, Part1) ->
  load(Rest, Part, Rows, Part1);
load([Line | Rest], Part, Rows, Part1) ->
  [Data | _] = string:split(Line, "#"),
  Cols = [[list_to_integer(H, 16) || H <- string:lexemes(C, " ")]
          || C <- lists:sublist(string:split(Data, ";", all), 5)],
  Part1b = case {Part, Cols} of
             {$1, [[CP] | _]} -> [CP | Part1];
             _ -> Part1
           end,
  load(Rest, Part, [Cols | Rows], Part1b).

lowercase_test() ->
  ?assertEqual("abc", idna_nfc:lowercase("ABC")),
  ?assertEqual([16#3B1], idna_nfc:lowercase([16#391])),
  %% Unconditional SpecialCasing mapping.
  ?assertEqual([16#69, 16#307], idna_nfc:lowercase([16#130])),
  %% A capital letter added in Unicode 18.0.0.
  ?assertEqual(idna_data:lowercase(16#A7DD), idna_nfc:lowercase([16#A7DD])),
  ?assertNotEqual([16#A7DD], idna_nfc:lowercase([16#A7DD])).
