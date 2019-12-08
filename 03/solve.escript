#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/3

main(Args) ->
  Wires = read_list("~s"),
  Tokens = [string:tokens(W, ",") || W <- Wires],
  Sol =
    case Args of
      ["2"] ->
        crosswires(Tokens, fun(_, V1, V2) -> V1 + V2 end);
      _ ->
        crosswires(Tokens, fun({X, Y}, _V1, _V2) -> abs(X) + abs(Y) end)
    end,
  io:format("~w~n", [Sol]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, [Res]} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

crosswires(Tokens, Fun) ->
  [Tokens1, Tokens2] = Tokens,
  Grid =
    #{ $1 => #{}
     , $2 => #{}
     , nearest => infinity
     , nfun => Fun
     },
  NewGrid = lay(Tokens1, Grid#{color => $1}),
  FinalGrid = lay(Tokens2, NewGrid#{color => $2}),
  #{nearest := D} = FinalGrid,
  D.

lay(Tokens, Grid) ->
  lay(Tokens, {0, 0}, Grid#{count => 1}).

lay([], _, Grid) ->
  Grid;
lay([Token|Rest], {X, Y}, Grid) ->
  [D|LStr] = Token,
  L = list_to_integer(LStr),
  {Wire, NP} =
    case D of
      $R ->
        {[{XX, Y} || XX <- lists:seq(X + 1, X + L,  1)], {X + L, Y}};
      $U ->
        {[{X, YY} || YY <- lists:seq(Y + 1, Y + L,  1)], {X, Y + L}};
      $L ->
        {[{XX, Y} || XX <- lists:seq(X - 1, X - L, -1)], {X - L, Y}};
      $D ->
        {[{X, YY} || YY <- lists:seq(Y - 1, Y - L, -1)], {X, Y - L}}
    end,
  NewGrid = update(Wire, Grid),
  lay(Rest, NP, NewGrid).

update([], Grid) ->
  Grid;
update([C|Rest], Grid) ->
  #{ $1 := One
   , $2 := Two
   , color := S
   , count := Count
   , nearest := N
   , nfun := NFun
   } = Grid,
  {Cs, Other} =
    case S of
      $1 -> {One, Two};
      $2 -> {Two, One}
    end,
  {NN, NCs} =
    case {maps:get(C, Cs, no), maps:get(C, Other, no)} of
      {no, no} ->
        {N, Cs#{C => Count}};
      {no, V2} ->
        M = min(N, NFun(C, Count, V2)),
        {M, Cs#{C => Count}};
      {_, _} ->
        {N, Cs}
    end,
  NewGrid =
    case S of
      $1 -> Grid#{$1 => NCs};
      $2 -> Grid#{$2 => NCs}
    end,
  update(Rest, NewGrid#{nearest := NN, count => Count + 1}).
