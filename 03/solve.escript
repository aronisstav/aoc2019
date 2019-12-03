#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/3

main(Args) ->
  Wires = read_list("~s"),
  Tokens = [string:tokens(W, ",") || W <- Wires],
  Sol =
    case Args of
      ["2"] -> -1;
      _ -> crosswires(Tokens)
    end,
  io:format("~w~n", [Sol]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, [Res]} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

crosswires(Tokens) ->
  [Tokens1, Tokens2] = Tokens,
  Grid = #{coords => #{}, nearest => infinity},
  NewGrid = lay(Tokens1, $1, Grid),
  FinalGrid = lay(Tokens2, $2, NewGrid),
  #{nearest := D} = FinalGrid,
  D.

lay(Tokens, S, Grid) ->
  lay(Tokens, S, {0, 0}, Grid).

lay([], _, _, Grid) ->
  Grid;
lay([Token|Rest], S, {X, Y}, Grid) ->
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
  NewGrid = update(Wire, S, Grid),
  lay(Rest, S, NP, NewGrid).

update([], _S, Grid) ->
  Grid;
update([{X, Y} = C|Rest], S, Grid) ->
  #{coords := Cs, nearest := N} = Grid,
  {NS, NN} =
    case maps:get(C, Cs, S) =:= S of
      true ->
        {S, N};
      false ->
        {$X, min(N, abs(X) + abs(Y))}
    end,
  %%io:format("~c ~p ~p~n", [NS, C, NN]),
  NC = Cs#{C => NS},
  NewGrid = Grid#{coords => NC, nearest => NN},
  update(Rest, S, NewGrid).
