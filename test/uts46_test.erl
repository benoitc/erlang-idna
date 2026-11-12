%% -*- coding: utf-8 -*-
%%%
%%% This file is part of erlang-idna released under the MIT license.
%%% See the LICENSE for more information.
%%%
%% UTS #46 conformance test against IdnaTestV2.txt.
%%
%% Every row is run through toUnicode (idna:decode/2), toAsciiN and toAsciiT
%% (idna:encode/2). A row with no status codes must give exactly the
%% expected string; a row with status codes must raise. The file is written
%% for CheckHyphens, CheckBidi, CheckJoiners, UseSTD3ASCIIRules and
%% VerifyDnsLength all set, which are the options used here.
%%
%% Two documented differences from the file are accepted:
%%
%% - Root label. idna keeps a trailing root label ("example.com.", an
%%   absolute name in RFC 1035) where UTS #46 VerifyDnsLength reports A4_2.
%%   A row whose only error is A4_2 is accepted when the result ends with
%%   the root dot.
%% - Long A-labels in toUnicode. idna always rejects labels over 63
%%   characters before Punycode decoding, to bound the decoding cost, where
%%   UTS #46 toUnicode does not check length. Accepted when toUnicode raises
%%   too_long and the row's toAsciiN status has A4_2.
-module(uts46_test).
-author("benoitc").

-include_lib("eunit/include/eunit.hrl").

uts46_conformance_test_() ->
  {timeout, 120, fun conformance/0}.

conformance() ->
  Rows = load_rows(),
  ?assert(length(Rows) > 6000),
  Failures = lists:flatmap(fun check_row/1, Rows),
  case Failures of
    [] -> ok;
    _ ->
      [?debugFmt("~p", [F]) || F <- lists:sublist(Failures, 20)],
      ?assertEqual({0, []}, {length(Failures), lists:sublist(Failures, 5)})
  end.

check_row(#{line := Line, source := Src} = Row) ->
  Ops = [{to_unicode, fun() -> idna:decode(Src, [uts46, std3_rules]) end,
          maps:get(to_unicode, Row), maps:get(to_unicode_status, Row)},
         {to_ascii_n, fun() -> idna:encode(Src, [uts46, std3_rules, {transitional, false}]) end,
          maps:get(to_ascii_n, Row), maps:get(to_ascii_n_status, Row)},
         {to_ascii_t, fun() -> idna:encode(Src, [uts46, std3_rules, {transitional, true}]) end,
          maps:get(to_ascii_t, Row), maps:get(to_ascii_t_status, Row)}],
  [{Op, Line, Expected, Status, Result}
   || {Op, Fun, Expected, Status} <- Ops,
      Result <- [run(Fun)],
      not conforms(Op, Expected, Status, Result, Row)].

run(Fun) ->
  try {ok, Fun()}
  catch Class:Reason -> {Class, Reason}
  end.

conforms(_Op, Expected, [], {ok, Expected}, _Row) -> true;
conforms(to_unicode, _Expected, [], {exit, {bad_label, {too_long, _}}}, Row) ->
  lists:member("A4_2", maps:get(to_ascii_n_status, Row));
conforms(_Op, _Expected, [], _Result, _Row) -> false;
conforms(_Op, _Expected, Status, {ok, Out}, _Row) ->
  Status -- ["A4_2"] =:= [] andalso Out =/= [] andalso lists:last(Out) =:= $.;
conforms(_Op, _Expected, _Status, {exit, _}, _Row) -> true;
conforms(_Op, _Expected, _Status, _Result, _Row) -> false.

%% Parse IdnaTestV2.txt following the FORMAT section of its header.
load_rows() ->
  EbinDir = filename:dirname(code:which(?MODULE)),
  Name = filename:join([filename:dirname(EbinDir), "test", "IdnaTestV2.txt"]),
  {ok, Bin} = file:read_file(Name),
  Lines = string:split(unicode:characters_to_list(Bin), "\n", all),
  [parse_row(L) || L <- Lines, L =/= "", hd(L) =/= $#].

parse_row(Line) ->
  [Data | _Comment] = string:split(Line, "#"),
  Cols0 = [string:trim(C) || C <- string:split(Data, ";", all)],
  [C1, C2, C3, C4, C5, C6, C7] = Cols0 ++ lists:duplicate(7 - length(Cols0), ""),
  Source = value(C1, ""),
  %% A blank value means the same as the column it derives from.
  ToUnicode = value(C2, Source),
  ToAsciiN = value(C4, ToUnicode),
  ToAsciiT = value(C6, ToAsciiN),
  %% A blank status inherits; an explicit [] means no errors.
  S3 = status(C3, []),
  S5 = status(C5, S3),
  S7 = status(C7, S5),
  #{line => string:trim(Line), source => Source,
    to_unicode => ToUnicode, to_unicode_status => S3,
    to_ascii_n => ToAsciiN, to_ascii_n_status => S5,
    to_ascii_t => ToAsciiT, to_ascii_t_status => S7}.

value("", Inherit) -> Inherit;
value("\"\"", _) -> "";
value(S, _) -> unescape(S).

status("", Inherit) -> Inherit;
status("[]", _) -> [];
status("[" ++ Rest, _) ->
  [string:trim(C) || C <- string:split(string:trim(Rest, trailing, "]"), ",", all)].

%% Decode the \uXXXX and \x{XXXX} escapes used in the file.
unescape([$\\, $u, A, B, C, D | Rest]) ->
  [list_to_integer([A, B, C, D], 16) | unescape(Rest)];
unescape([$\\, $x, ${ | Rest]) ->
  {Hex, [$} | Rest2]} = lists:splitwith(fun(C) -> C =/= $} end, Rest),
  [list_to_integer(Hex, 16) | unescape(Rest2)];
unescape([C | Rest]) ->
  [C | unescape(Rest)];
unescape([]) ->
  [].
