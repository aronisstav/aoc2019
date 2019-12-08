#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/8

%% Use as "<part> <width> <height>"

main([Part| DimensionsS]) ->
  {ok, [Input]} = io:fread("", "~s"),
  [Width, Height] = [list_to_integer(S) || S <- DimensionsS],
  Layers = layers(Input, Width * Height),
  Sol =
    case Part of
      "2" ->
        Linear = render(Layers),
        show(Linear, Width);
      "1" -> checksum(Layers)
    end,
  io:format("~w~n", [Sol]).

layers(Input, L) ->
  layers(Input, L, []).

layers([], _L, Layers) ->
  Layers;
layers(Input, L, Layers) ->
  {NL, Rest} = lists:split(L, Input),
  layers(Rest, L, [NL|Layers]).

checksum(Layers) ->
  checksum(Layers, infinity, 0).

checksum([], _, Sum) -> Sum;
checksum([L|Rest], Min, Sum) ->
  Zero = length([D || D <- L, D == $0]),
  {NMin, NSum} =
    case Zero < Min of
      true ->
        One = length([D || D <- L, D == $1]),
        Two = length([D || D <- L, D == $2]),
        {Zero, One * Two};
      false ->
        {Min, Sum}
    end,
  checksum(Rest, NMin, NSum).

render([L|Layers]) ->
  render(Layers, L).

render([], F) -> F;
render([L|Rest], F) ->
  NF = lists:zipwith(fun lay/2, L, F),
  render(Rest, NF).

lay($2, X) -> X;
lay(X, _) -> X.

show([], _) -> ok;
show(L, W) ->
  {L1,R} = lists:split(W, L),
  V = [map(C) || C <- L1],
  io:format("~s~n", [V]),
  show(R, W).

map($1) -> $*;
map($0) -> $ .
