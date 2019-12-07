#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/6

main(Args) ->
  Input = read_list("~s"),
  Data = make_orbits(Input),
  Sol =
    case Args of
      ["2"] -> you_san(Data);
      _ -> total(Data)
    end,
  io:format("~w~n", [Sol]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

make_orbits(Input) ->
  make_orbits(Input, #{}).

make_orbits([], Map) -> Map;
make_orbits([[Line]|Rest], Map) ->
  [Center, Orbiter] = string:tokens(Line, ")"),
  NewMap = Map#{Orbiter => Center},  
  make_orbits(Rest, NewMap).

total(Data) ->
  Keys = maps:keys(Data),
  total(Keys, Data, 0).

total([], _, N) -> N;
total([Key|Rest], Data, N) ->
  I = find_root(Key, Data),
  total(Rest, Data, N + I).

find_root(Key, Data) ->
  find_root(Key, Data, 0).

find_root(Key, Data, N) ->
  case maps:get(Key, Data, none) of
    none -> N;
    New  -> find_root(New, Data, N + 1)
  end.

you_san(Data) ->
  YouPath = path("YOU", Data),
  SanPath = path("SAN", Data),
  YouNique = length(YouPath -- SanPath),
  SanNique = length(SanPath -- YouPath),
  YouNique + SanNique - 2.

path(Target, Data) ->
  path(Target, Data, [Target]).

path(Target, Data, Acc) ->
  case maps:get(Target, Data, none) of
    none -> Acc;
    New  -> path(New, Data, [New|Acc])
  end.
