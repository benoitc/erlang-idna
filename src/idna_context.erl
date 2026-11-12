%% -*- coding: utf-8 -*-
%%%
%%% This file is part of erlang-idna released under the MIT license.
%%% See the LICENSE for more information.
%%%
%% @doc Contextual rules of RFC 5892 Appendix A (CONTEXTJ and CONTEXTO).
%%
%% The `_at' functions take the code point, the code points before it in
%% reverse order and the code points after it, so a label is walked once
%% instead of being indexed for every contextual character.
-module(idna_context).
-author("benoitc").

%% API
-export([
  valid_contextj/2, valid_contextj/3,
  valid_contexto/2, valid_contexto/3,
  valid_contextj_at/3,
  valid_contexto_at/4,
  label_facts/1,
  contexto_with_rule/1
]).

-define(virama_combining_class, 9).

-type facts() :: #{japanese := boolean(),
                   arabic_indic := boolean(),
                   ext_arabic_indic := boolean()}.
-export_type([facts/0]).

%% @doc Check the CONTEXTJ rule for the code point at `Pos' (0-based).
valid_contextj([], _Pos) -> true;
valid_contextj(Label, Pos) ->
  CP = lists:nth(Pos + 1, Label),
  valid_contextj(CP, Label, Pos).

valid_contextj(CP, Label, Pos) ->
  {Before, [_ | After]} = lists:split(Pos, Label),
  valid_contextj_at(CP, lists:reverse(Before), After).

%% @doc Check the CONTEXTJ rule for `CP', given the code points before it
%% (nearest first) and after it.
valid_contextj_at(16#200C, RevBefore, After) ->
  %% RFC 5892 A.1 ZERO WIDTH NON-JOINER:
  %%   If Canonical_Combining_Class(Before(cp)) .eq. Virama Then True;
  %%   If RegExpMatch((Joining_Type:{L,D})(Joining_Type:T)*‌
  %%                  (Joining_Type:T)*(Joining_Type:{R,D})) Then True;
  virama_before(RevBefore)
    orelse (joining_side(RevBefore, ["L", "D"])
            andalso joining_side(After, ["R", "D"]));
valid_contextj_at(16#200D, RevBefore, _After) ->
  %% RFC 5892 A.2 ZERO WIDTH JOINER:
  %%   If Canonical_Combining_Class(Before(cp)) .eq. Virama Then True;
  virama_before(RevBefore);
valid_contextj_at(_, _, _) ->
  false.

virama_before([Prev | _]) ->
  idna_data:ccc(Prev) =:= ?virama_combining_class;
virama_before([]) ->
  false.

%% Skip transparent (T) code points, then the first other code point must
%% have one of the wanted joining types.
joining_side([CP | Rest], Wanted) ->
  case joining_type(CP) of
    "T" -> joining_side(Rest, Wanted);
    Type -> lists:member(Type, Wanted)
  end;
joining_side([], _Wanted) ->
  false.

%% ArabicShaping.txt only lists explicit joining types. Code points not
%% listed there have type T when their General_Category is Mn, Me or Cf,
%% and U otherwise.
joining_type(CP) ->
  case idna_data:joining_types(CP) of
    undefined ->
      case idna_data:lookup(CP) of
        {GC, _} when GC =:= "Mn"; GC =:= "Me"; GC =:= "Cf" -> "T";
        _ -> "U"
      end;
    Type ->
      Type
  end.

%% @doc Check the CONTEXTO rule for the code point at `Pos' (0-based).
valid_contexto([], _Pos) ->
  true;
valid_contexto(Label, Pos) ->
  CP = lists:nth(Pos + 1, Label),
  valid_contexto(CP, Label, Pos).

valid_contexto(CP, Label, Pos) ->
  {Before, [_ | After]} = lists:split(Pos, Label),
  valid_contexto_at(CP, lists:reverse(Before), After, Label).

%% @doc Check the CONTEXTO rule for `CP'. `LabelOrFacts' is either the
%% whole label or the result of {@link label_facts/1} for it; pass the
%% facts when checking several code points of the same label.
-spec valid_contexto_at(non_neg_integer(), [non_neg_integer()], [non_neg_integer()],
                        [non_neg_integer()] | facts()) -> boolean().
valid_contexto_at(16#00B7, RevBefore, After, _) ->
  %% A.3 MIDDLE DOT: Before(cp) .eq. U+006C And After(cp) .eq. U+006C
  case {RevBefore, After} of
    {[16#006C | _], [16#006C | _]} -> true;
    _ -> false
  end;
valid_contexto_at(16#0375, _RevBefore, After, _) ->
  %% A.4 GREEK LOWER NUMERAL SIGN (KERAIA): Script(After(cp)) .eq. Greek
  case After of
    [Next | _] -> idna_data:scripts(Next) =:= "greek";
    [] -> false
  end;
valid_contexto_at(CP, RevBefore, _After, _) when CP == 16#05F3; CP == 16#05F4 ->
  %% A.5/A.6 HEBREW PUNCTUATION GERESH/GERSHAYIM: Script(Before(cp)) .eq. Hebrew
  case RevBefore of
    [Prev | _] -> idna_data:scripts(Prev) =:= "hebrew";
    [] -> false
  end;
valid_contexto_at(16#30FB, _RevBefore, _After, LabelOrFacts) ->
  %% A.7 KATAKANA MIDDLE DOT: some code point of the label is Hiragana,
  %% Katakana or Han.
  maps:get(japanese, facts(LabelOrFacts));
valid_contexto_at(CP, _RevBefore, _After, LabelOrFacts) when CP >= 16#0660, CP =< 16#0669 ->
  %% A.8 ARABIC-INDIC DIGITS: no code point in 06F0..06F9 in the label.
  not maps:get(ext_arabic_indic, facts(LabelOrFacts));
valid_contexto_at(CP, _RevBefore, _After, LabelOrFacts) when CP >= 16#06F0, CP =< 16#06F9 ->
  %% A.9 EXTENDED ARABIC-INDIC DIGITS: no code point in 0660..0669 in the label.
  not maps:get(arabic_indic, facts(LabelOrFacts));
valid_contexto_at(_, _, _, _) ->
  false.

facts(Facts) when is_map(Facts) -> Facts;
facts(Label) -> label_facts(Label).

%% @doc Label-wide facts used by the CONTEXTO rules, computed in one pass.
-spec label_facts([non_neg_integer()]) -> facts().
label_facts(Label) ->
  lists:foldl(
    fun(CP, Acc) when CP >= 16#0660, CP =< 16#0669 -> Acc#{arabic_indic := true};
       (CP, Acc) when CP >= 16#06F0, CP =< 16#06F9 -> Acc#{ext_arabic_indic := true};
       (CP, #{japanese := false} = Acc) ->
        case idna_data:scripts(CP) of
          S when S =:= "hiragana"; S =:= "katakana"; S =:= "han" -> Acc#{japanese := true};
          _ -> Acc
        end;
       (_, Acc) -> Acc
    end,
    #{japanese => false, arabic_indic => false, ext_arabic_indic => false},
    Label).

contexto_with_rule(16#00B7) -> true;
% MIDDLE DOT
contexto_with_rule(16#0375) -> true;
% GREEK LOWER NUMERAL SIGN (KERAIA)
contexto_with_rule(16#05F3) -> true;
% HEBREW PUNCTUATION GERESH
contexto_with_rule(16#05F4) -> true;
% HEBREW PUNCTUATION GERSHAYIM
contexto_with_rule(16#30FB) -> true;
% KATAKANA MIDDLE DOT
contexto_with_rule(CP) when 16#0660 =< CP, CP =< 16#0669 -> true;
% ARABIC-INDIC DIGITS
contexto_with_rule(CP) when 16#06F0 =< CP, CP =< 16#06F9 -> true;
% EXTENDED ARABIC-INDIC DIGITS
contexto_with_rule(_) -> false.
