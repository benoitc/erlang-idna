#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

-mode(compile).

-define(MOD, "idna_data").

-export([main/1]).

-ifdef('OTP_RELEASE').
-define(chomp(Str), string:chomp(Str)).
-define(trim(Str, Dir), string:trim(Str, Dir)).
-define(lexemes(Str, Pat), string:lexemes(Str, Pat)).
-define(lower(C), string:lowercase(C)).
-else.
-define(chomp(Str), string:strip(Str, right, $\n)).
-define(trim(Str, Dir), string:strip(Str, Dir)).
-define(lexemes(Str, Pat), string:strip(string:tokens(Str, Pat), both)).
-define(lower(C), string:to_lower(C)).
-endif.

%% Default `Bidi_Class` for unassigned codepoints.
%%
%% Ref: <https://www.unicode.org/Public/UNIDATA/extracted/DerivedBidiClass.txt>
-define(BIDI_CLASS_DEFAULTS, [
  %% The @missing lines of DerivedBidiClass-18.0.0.txt.
  {{16#0600, 16#07BF}, "AL"},
  {{16#0860, 16#08FF}, "AL"},
  {{16#FB50, 16#FDCF}, "AL"},
  {{16#FDF0, 16#FDFF}, "AL"},
  {{16#FE70, 16#FEFF}, "AL"},
  {{16#00010D00, 16#00010D3F}, "AL"},
  {{16#00010EC0, 16#00010EFF}, "AL"},
  {{16#00010F30, 16#00010F6F}, "AL"},
  {{16#0001EC70, 16#0001ECBF}, "AL"},
  {{16#0001ED00, 16#0001ED4F}, "AL"},
  {{16#0001EE00, 16#0001EEFF}, "AL"},
  %% Arabic, Syriac, and Thaana blocks, among others
  {{16#0590, 16#05FF}, "R"},
  {{16#07C0, 16#085F}, "R"},
  {{16#FB1D, 16#FB4F}, "R"},
  {{16#00010800, 16#00010CFF}, "R"},
  {{16#00010D40, 16#00010EBF}, "R"},
  {{16#00010F00, 16#00010F2F}, "R"},
  {{16#00010F70, 16#00010FFF}, "R"},
  {{16#0001E800, 16#0001EC6F}, "R"},
  {{16#0001ECC0, 16#0001ECFF}, "R"},
  {{16#0001ED50, 16#0001EDFF}, "R"},
  {{16#0001EF00, 16#0001EFFF}, "R"},
  %% Hebrew, NKo, and Phoenician blocks, among others.
  {{16#20A0, 16#20CF}, "ET"}
  %% Currency Symbols block.
]).

main(_) ->
  {ok, IM} = file:open("../uc_spec/UnicodeData.txt", [read, raw, {read_ahead, 1000000}]),
  Data = foldl(fun parse_unicode_data/2, [], IM),
  file:close(IM),

  {ok, AS} = file:open("../uc_spec/ArabicShaping.txt", [read, raw, {read_ahead, 1000000}]),
  JoiningTypes = foldl(fun parse_as/2, [], AS),
  ok = file:close(AS),

  {ok, ScriptsF} = file:open("../uc_spec/Scripts.txt", [read, raw, {read_ahead, 1000000}]),
  Scripts = foldl(fun parse_scripts/2, [], ScriptsF),
  ok = file:close(ScriptsF),

  %% Normalization and case data, so idna does not depend on the Unicode
  %% version of the OTP release it runs on.
  {ok, UD} = file:open("../uc_spec/UnicodeData.txt", [read, raw, {read_ahead, 1000000}]),
  Props = foldl(fun parse_props/2, [], UD),
  ok = file:close(UD),
  {ok, CE} = file:open("../uc_spec/CompositionExclusions.txt", [read, raw, {read_ahead, 1000000}]),
  Exclusions = foldl(fun parse_code_point_line/2, [], CE),
  ok = file:close(CE),
  {ok, SC} = file:open("../uc_spec/SpecialCasing.txt", [read, raw, {read_ahead, 1000000}]),
  SpecialLower = foldl(fun parse_special_lower/2, [], SC),
  ok = file:close(SC),

  %% Make module
  OutputPath = filename:join(["..", "src", ?MOD++".erl"]),
  {ok, Out} = file:open(OutputPath, [write]),
  gen_file(Out, Data, JoiningTypes, Scripts, Props, Exclusions, SpecialLower),
  ok = file:close(Out),
  ok.

gen_file(Fd, Data, JoiningTypes, Scripts, Props, Exclusions, SpecialLower) ->
  gen_header(Fd),
  gen_bidirectional(Fd),
  gen_lookup(Fd, Data),
  gen_joining_types(Fd, JoiningTypes),
  gen_scripts_types(Fd, Scripts),
  gen_normalization(Fd, Props, Exclusions),
  gen_lowercase(Fd, Props, SpecialLower),
  ok.

%% ccc/1, decomposition/1 (full canonical decomposition) and compose/2
%% (primary composites, without the Full_Composition_Exclusion ones).
gen_normalization(Fd, Props, Exclusions) ->
  CCC = maps:from_list([{CP, C} || {CP, C, _, _} <- Props, C =/= 0]),
  Decomp = maps:from_list([{CP, D} || {CP, _, D, _} <- Props, D =/= []]),
  lists:foreach(
    fun({CP, C}) -> io:format(Fd, "ccc(~w) -> ~w;~n", [CP, C]) end,
    lists:sort(maps:to_list(CCC))),
  io:put_chars(Fd, "ccc(_) -> 0.\n\n"),
  lists:foreach(
    fun({CP, _}) ->
        io:format(Fd, "decomposition(~w) -> ~w;~n", [CP, full_decomposition(CP, Decomp)])
    end,
    lists:sort(maps:to_list(Decomp))),
  io:put_chars(Fd, "decomposition(_) -> [].\n\n"),
  Excluded = sets:from_list(Exclusions),
  Composites = [{A, B, CP} || {CP, [A, B]} <- lists:sort(maps:to_list(Decomp)),
                              not sets:is_element(CP, Excluded),
                              maps:get(CP, CCC, 0) =:= 0,
                              maps:get(A, CCC, 0) =:= 0],
  lists:foreach(
    fun({A, B, CP}) -> io:format(Fd, "compose(~w, ~w) -> ~w;~n", [A, B, CP]) end,
    Composites),
  io:put_chars(Fd, "compose(_, _) -> false.\n\n").

full_decomposition(CP, Decomp) ->
  case maps:find(CP, Decomp) of
    {ok, D} -> lists:append([full_decomposition(C, Decomp) || C <- D]);
    error -> [CP]
  end.

%% lowercase/1: the unconditional lowercase mapping of SpecialCasing.txt,
%% or else the simple lowercase mapping of UnicodeData.txt.
gen_lowercase(Fd, Props, SpecialLower) ->
  Simple = maps:from_list([{CP, [L]} || {CP, _, _, L} <- Props, L =/= undefined]),
  Lower = maps:merge(Simple, maps:from_list(SpecialLower)),
  lists:foreach(
    fun({CP, L}) when L =/= [CP] -> io:format(Fd, "lowercase(~w) -> ~w;~n", [CP, L]);
       (_) -> ok
    end,
    lists:sort(maps:to_list(Lower))),
  io:put_chars(Fd, "lowercase(CP) -> [CP].\n\n").


gen_header(Fd) ->
  io:put_chars(Fd, "%%\n%% this file is generated do not modify\n"),
  io:put_chars(Fd, "%% see ../uc_spec/gen_idnadata_mod.escript\n\n"),
  io:put_chars(Fd, "-module(" ++ ?MOD ++").\n"),
  io:put_chars(Fd, "-compile(compressed).\n"),
  io:put_chars(Fd, "-export([lookup/1, joining_types/1, scripts/1]).\n"),
  io:put_chars(Fd, "-export([bidirectional/1]).\n"),
  io:put_chars(Fd, "-export([ccc/1, decomposition/1, compose/2, lowercase/1]).\n"),
  ok.

gen_lookup(Fd, Data) ->
  lists:foreach(
    fun({Cp,Tp}) ->
      io:format(Fd, "lookup(~w) -> ~p;~n", [Cp, Tp])
    end,
    lists:sort(Data)
  ),
  io:put_chars(Fd, "lookup(_) -> false.\n\n").

gen_bidirectional(Fd) ->
  io:put_chars(Fd, "bidirectional(CP) ->\n"),
  io:put_chars(Fd, "  case lookup(CP) of \n"),
  io:put_chars(Fd, "    {_, C} -> C;\n"),
  io:put_chars(Fd, "    false -> bidirectional_1(CP)\n"),
  io:put_chars(Fd, "  end.\n\n"),
  lists:foreach(
    fun({Cp, Class}) ->
      io:format(Fd, "bidirectional_1~s ~p;~n", [gen_single_clause(Cp), Class])
    end,
    lists:sort(?BIDI_CLASS_DEFAULTS)
  ),
  io:put_chars(Fd, "bidirectional_1(_) -> \"L\".\n\n").

gen_joining_types(Fd, JoiningTypes) ->
  lists:foreach(
    fun({Cp, Jt}) ->
      io:format(Fd, "joining_types(~w) -> ~p;~n", [Cp, ?trim(Jt, both)])
    end,
    lists:sort(JoiningTypes)
  ),
  io:put_chars(Fd, "joining_types(_) -> undefined.\n\n").

gen_scripts_types(Fd, Scripts) ->
  lists:foreach(
    fun({Cp, Jt}) ->
      io:format(Fd, "scripts~s ~p;~n", [gen_single_clause(Cp), ?lower(?trim(Jt, both))])
    end,
    optimize_scripts_ranges(lists:sort(Scripts))
  ),
  io:put_chars(Fd, "scripts(_) -> false.\n\n").

optimize_scripts_ranges(Rs0) ->
  PF = fun
         ({{N, undefined}, _}) when is_integer(N) -> true;
         (_) -> false
       end,

  {Singles, Rs} = lists:partition(PF, Rs0),
  Singles ++ Rs.


gen_single_clause({R0, undefined}) ->
  io_lib:format("(~w) ->", [R0]);
gen_single_clause({R0, R1}) ->
  io_lib:format("(CP) when ~w =< CP, CP =< ~w ->", [R0,R1]).



parse_unicode_data(Line0, Acc) ->
  Line = ?chomp(Line0),
  [CodePoint, _Name, Cat, _Class, BiDi |_] = tokens(Line, ";"),
  [{hex_to_int(CodePoint), {?trim(Cat, both), ?trim(BiDi, both)}} | Acc].

%% {CodePoint, CanonicalCombiningClass, CanonicalDecomposition, SimpleLowercase}
parse_props(Line0, Acc) ->
  Line = ?chomp(Line0),
  [CodePoint, _Name, _Cat, Class, _BiDi, Decomp | Rest] = tokens(Line, ";"),
  Canonical = case ?trim(Decomp, both) of
                "" -> [];
                "<" ++ _ -> [];
                D -> [hex_to_int(H) || H <- ?lexemes(D, " ")]
              end,
  Lower = case lists:nth(8, Rest) of
            "" -> undefined;
            L -> hex_to_int(L)
          end,
  [{hex_to_int(CodePoint), list_to_integer(?trim(Class, both)), Canonical, Lower} | Acc].

%% CompositionExclusions.txt: one code point per line.
parse_code_point_line(Line0, Acc) ->
  [Line | _] = tokens(?chomp(Line0), "#"),
  case ?trim(Line, both) of
    "" -> Acc;
    CP -> [hex_to_int(CP) | Acc]
  end.

%% SpecialCasing.txt: keep unconditional lowercase mappings only.
parse_special_lower(Line0, Acc) ->
  [Line | _] = tokens(?chomp(Line0), "#"),
  case [?trim(F, both) || F <- tokens(Line, ";")] of
    [CP, Lower, _Title, _Upper, ""] ->
      [{hex_to_int(CP), [hex_to_int(H) || H <- ?lexemes(Lower, " ")]} | Acc];
    _ ->
      Acc
  end.

parse_as(Line0, Acc) ->
  Line = ?chomp(Line0),
  case tokens(Line, ";") of
    [CodePoint, _,JT|_] ->
      [{hex_to_int(CodePoint), ?trim(JT, both) } | Acc];
    _ ->
      Acc
  end.

parse_scripts(Line0, Acc) ->
  [Line|_Comments] = tokens(Line0, "#"),
  [CodePoints, Script0] = tokens(Line, ";"),
  Script1 = ?trim(Script0, both),
  case lists:member(Script1, ["Greek", "Han", "Hebrew", "Hiragana", "Katakana"]) of
    true ->
      [{to_range(CodePoints), Script1} | Acc];
    false ->
      Acc
  end.


to_range(CodePoints0) ->
  case tokens(CodePoints0, ".") of
    [CodePoint] ->
      {hex_to_int(CodePoint), undefined};
    [CodePoint1, "", CodePoint2] ->
      {hex_to_int(CodePoint1), hex_to_int(CodePoint2)}
  end.

hex_to_int([]) -> [];
hex_to_int(HexStr) ->
  list_to_integer(?trim(HexStr, both), 16).


foldl(Fun, Acc, Fd) ->
  Get = fun() -> file:read_line(Fd) end,
  foldl_1(Fun, Acc, Get).

foldl_1(_Fun, {done, Acc}, _Get) -> Acc;
foldl_1(Fun, Acc, Get) ->
  case Get() of
    eof -> Acc;
    {ok, "#" ++ _} -> %% Ignore comments
      foldl_1(Fun, Acc, Get);
    {ok, "\n"} -> %% Ignore empty lines
      foldl_1(Fun, Acc, Get);
    {ok, Line} ->
      foldl_1(Fun, Fun(Line, Acc), Get)
  end.



%% Differs from string:tokens, it returns empty string as token between two delimiters
tokens(S, [C]) ->
  tokens(lists:reverse(S), C, []).

tokens([Sep|S], Sep, Toks) ->
  tokens(S, Sep, [[]|Toks]);
tokens([C|S], Sep, Toks) ->
  tokens_2(S, Sep, Toks, [C]);
tokens([], _, Toks) ->
  Toks.

tokens_2([Sep|S], Sep, Toks, Tok) ->
  tokens(S, Sep, [Tok|Toks]);
tokens_2([C|S], Sep, Toks, Tok) ->
  tokens_2(S, Sep, Toks, [C|Tok]);
tokens_2([], _Sep, Toks, Tok) ->
  [Tok|Toks].
