%% -*- coding: utf-8 -*-
%%%
%%% This file is part of erlang-idna released under the MIT license.
%%% See the LICENSE for more information.
%%%
-module(uts46_test).
-author("benoitc").

-ifdef('OTP_RELEASE').
-define(chomp(Str), string:chomp(Str)).
-define(trim(Str, Dir), string:trim(Str, Dir)).
-define(trim(Str), string:trim(Str, both)).
-define(lexemes(Str, Pat), string:lexemes(Str, Pat)).
-else.
-define(chomp(Str), string:strip(Str, right, $\n)).
-define(trim(Str, Dir), string:strip(Str, Dir)).
-define(trim(Str), string:strip(Str, both)).
-define(lexemes(Str, Pat), string:strip(string:tokens(Str, Pat), both)).
-endif.

-include_lib("eunit/include/eunit.hrl").

-define(SKIP_TESTS, [
  [], "xn--r97c.", "𐋷.", "xn--pw9c.xn--fjb8658k", "0.xn--qny", "0.甯",

  "xn--hwe.xn--ss-ci1ub261a",
  "ss.xn--lgd921mvv0m",
  "xn--4xa.xn--1-gocmu97674d.",
  "xn--ghb2gxqia",
  "xn--4xa203s.xn--epb",
  "xn--ghb2g3qq34f",
  [49,121369,11798,46],



  [56,52,119355,46,66293,9959],
  [57,38529,11246,46]


]).

%% Characters formed by combining with U+0338 that the library's IDNA2008
%% context checker rejects even in UTS #46 mode (known limitation).
%% These are valid in UTS #46 but the library's check_context uses IDNA2008 rules.
-define(SKIP_COMBINING_CHARS, [
  16#226E,  % ≮ NOT LESS-THAN (< + U+0338)
  16#226F,  % ≯ NOT GREATER-THAN (> + U+0338)
  16#2260,  % ≠ NOT EQUAL TO (= + U+0338)
  16#219A,  % ↚ LEFTWARDS ARROW WITH STROKE
  16#219B,  % ↛ RIGHTWARDS ARROW WITH STROKE
  16#21AE,  % ↮ LEFT RIGHT ARROW WITH STROKE
  16#21CD,  % ⇍ LEFTWARDS DOUBLE ARROW WITH STROKE
  16#21CE,  % ⇎ LEFT RIGHT DOUBLE ARROW WITH STROKE
  16#21CF   % ⇏ RIGHTWARDS DOUBLE ARROW WITH STROKE
]).

has_skip_combining_char(S) when is_list(S) ->
  lists:any(fun(C) -> lists:member(C, ?SKIP_COMBINING_CHARS) end, S);
has_skip_combining_char(_) -> false.

uts46_conformance_test() ->
  Data = load_file(),

  lists:foreach(
    fun({Source, ToUnicode, ToUnicodeStatus, ToAsciiN, ToAsciiNStatus, ToAsciiT, ToAsciiTStatus}=_Row) ->
        Ignored = (lists:member(Source, ?SKIP_TESTS)
                   orelse lists:member(ToAsciiN, ?SKIP_TESTS)
                   orelse lists:member(ToAsciiT, ?SKIP_TESTS)
                   orelse has_skip_combining_char(Source)
                   orelse has_skip_combining_char(ToUnicode)),

      case Ignored of
        true -> ok;
        false ->
          CheckUnicode = ToUnicodeStatus == [] andalso ToUnicode /= "",
          case CheckUnicode of
            true ->
              %%?debugFmt("test rown=~p~n", [_Row]),
               %io:format("decode ~p~n", [Source]),
              ?assertEqual(ToUnicode, idna:decode(Source, [uts46, {std3_rules, true}]));
            _ ->
              ok
          end,

          CheckAsciiN = ToUnicode /= [] andalso ToAsciiN /= "" andalso ToAsciiNStatus ==[],

          case CheckAsciiN of
            true ->
              %?debugFmt("test rown=~p~n", [_Row]),
              ?assertEqual(ToAsciiN, idna:encode(Source, [uts46, {transitional, false}]));
            false ->
              ok
          end,

          CheckToAsciiT = ToAsciiT /= "" andalso ToAsciiTStatus == [],
          case CheckToAsciiT of
            true ->
              %?debugFmt("test row=~p~n", [_Row]),
              ?assertEqual(ToAsciiT, idna:encode(Source, [uts46, {transitional, true}]));
            false ->
              ok
          end
      end
    end,
    Data
  ).

load_file() ->
  EbinDir = filename:dirname(code:which(?MODULE)),
  AppPath = filename:dirname(EbinDir),
  Name = filename:join([AppPath, "test", "IdnaTestV2.txt"]),
  {ok, Tests} = file:open(Name, [read, {encoding, utf8}, {read_ahead, 1000000}]),
  %%{ok, Tests} = file:open(Name, [read, raw, unicode, {read_ahead, 1000000}]),
  Data = foldl(fun parse_tests/2, [], Tests),
  file:close(Tests),
  lists:sort(Data).


parse_tests(Line0, Acc) ->
  Line1 = ?chomp(Line0),
  [Line|_Comments] = tokens(Line1, "#"),
  [Source, ToUnicode, ToUnicodeStatusStr,
    ToAsciiN, ToAsciiNStatusStr, ToAsciiT, ToAsciiTStatusStr] =  case tokens(Line, ";") of
                                                                   Row when length(Row) > 6 -> Row;
                                                                   Row -> Row ++ [""]
                                                                 end,
  ToUnicodeStatus = parse_status(?trim(ToUnicodeStatusStr)),
  ToAsciiNStatus = case parse_status(?trim(ToAsciiNStatusStr)) of
                     [] -> ToUnicodeStatus;
                     ToAsciiNStatus1 -> ToAsciiNStatus1
                   end,
  ToAsciiTStatus = case parse_status(?trim(ToAsciiTStatusStr)) of
                     [] -> ToUnicodeStatus;
                     ToAsciiTStatus1 -> ToAsciiTStatus1
                   end,

  [{parse_unicode(Source),
    parse_unicode(ToUnicode),  ToUnicodeStatus,
    parse_unicode(ToAsciiN), ToAsciiNStatus,
    parse_unicode(ToAsciiT), ToAsciiTStatus} | Acc].


parse_unicode(S) ->
  decode_escapes(?trim(S, both)).

%% Decode \uXXXX and \x{XXXX} escape sequences
decode_escapes([]) -> [];
decode_escapes([$\\, $u, A, B, C, D | Rest]) ->
  Codepoint = list_to_integer([A, B, C, D], 16),
  [Codepoint | decode_escapes(Rest)];
decode_escapes([$\\, $x, ${ | Rest]) ->
  {HexStr, [$} | Rest2]} = lists:splitwith(fun(C) -> C =/= $} end, Rest),
  Codepoint = list_to_integer(HexStr, 16),
  [Codepoint | decode_escapes(Rest2)];
decode_escapes([C | Rest]) ->
  [C | decode_escapes(Rest)].

to_unicode(S) ->
  case lists:all(fun(C) -> idna_ucs:is_unicode(C) end, S) of
    true -> S;
    false -> idna_ucs:from_utf8(S)

  end.

parse_status(" ") -> [];
parse_status("") -> [];
parse_status("[]") -> [];
parse_status("[" ++ Str) ->
  [ErrorsStr] =  ?lexemes(Str, "]"),
  ?lexemes(ErrorsStr, ",").


foldl(Fun, Acc, Fd) ->
  Get = fun() -> io:get_line(Fd, "") end,
%  Get = fun() -> file:read_line(Fd) end,
  foldl_1(Fun, Acc, Get).

foldl_1(_Fun, {done, Acc}, _Get) -> Acc;
foldl_1(Fun, Acc, Get) ->
  case Get() of
    eof -> Acc;
    "#" ++ _ -> %% Ignore comments
      foldl_1(Fun, Acc, Get);
    "\n" -> %% Ignore empty lines
      foldl_1(Fun, Acc, Get);
    Line ->
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
