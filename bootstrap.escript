#!/usr/bin/env escript
%%%
%%% This file is part of erlang-idna released under the BSD license.
%%% See the LICENSE for more information.
%%%

-define(URL, "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt").
-define(PATH, "src/dna_unicode_data.hrl").
-define(HEADER, "%%% -*- erlang -*-
%%%
%%% This file is part of erlang-idna released under the BSD license.
%%% See the LICENSE for more information.
%%%

").

main([])->
    inets:start(),
    Data = download(?URL),
    generate(?PATH, Data).


download(Url)->
    io:fwrite("=> Downloading the Unicode data: ~p...", [?URL]),
    {ok, {_, _, Data}} = httpc:request(Url),
    io:fwrite("[ok]~n", []),
    list_to_binary(Data).

generate(Path, Data)->
    io:fwrite("=> Parsing the data...", []),
    Items = parse(Data),
    io:fwrite("[ok]~n", []),
    {ok, File} = file:open(Path, [write]),
    write_header(File),
    write_data(File, Items).


parse(Data) ->
    parse(Data, []).

parse(<<>>, Acc) ->
    lists:reverse(Acc);
parse(Data, Acc) ->
    {Line, Etc} = break_at($\n, Data),
    Values = re:split(Line, ";", [{return, list}]),
    parse(Etc, [list_to_tuple(Values)|Acc]).

break_at(C, Data) ->
    break_at(C, Data, []).

break_at(_, <<>>, Prefix) ->
    {lists:reverse(Prefix), <<>>};
break_at(C, <<C, T/bytes>>, Prefix) ->
    {lists:reverse(Prefix), T};
break_at(C, <<H, T/bytes>>, Prefix) ->
    break_at(C, T, [H | Prefix]).

dehex(Strings) ->
    [erlang:list_to_integer(String, 16) || String <- Strings].

write_header(File)->
    file:write(File, ?HEADER).

write_data(File, Items)->
    io:fwrite("=> Writing Unicode codepoint funs...", []),
    write_codepoints(File, Items),
    io:fwrite("[ok]~n", []),
    io:fwrite("=> Writing decomposition funs...", []),
    write_decompositions(File, Items),
    io:fwrite("[ok]~n", []).

write_codepoints(File, Items)->
    lists:foreach(
        fun(Item)->
            HexCP = element(1, Item),
            [CP] = dehex([HexCP]),
            io:fwrite(File, "
codepoint(~p)->
    {ok, ~p};", [CP, Item])
        end, Items),
    file:write(File, "
codepoint(_)->
    {error, bad_codepoint}.").

write_decompositions(File, Items)->
    lists:foreach(
        fun(Item)->
            Decomp = element(6, Item),
            case re:run(Decomp, "^[0-9A-F]{4} [0-9A-F]{4}$") of
                {match, _}->
                    io:fwrite(File, "
decomposition(~p)->
    {ok, ~p};", [Decomp, Item]);
                nomatch->
                    ok
            end
        end, Items),
    file:write(File, "
decomposition(_)->
    {error, bad_codepoint}.").
