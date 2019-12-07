#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/4

%% Part N: run as "N <low> <high>"

main([Mode | Limits]) ->
  [Low, High] = [list_to_integer(N) || N <- Limits],
  Sol =
    case Mode of
      "1" -> count(fun(I) -> nondesc(I) andalso repeated(I) end, Low, High);
      "2" -> count(fun(I) -> nondesc(I) andalso double(I) end, Low, High)
    end,
  io:format("~w~n", [Sol]).

count(Fun, Low, High) ->
  count(Low, High, Fun, 0).

count(I, High, _, N) when I > High ->
    N;
count(I, High, Fun, N) ->
    NN =
        case Fun(I) of
            true -> N + 1;
            false -> N
        end,
    count(I + 1, High, Fun, NN).

nondesc(N) ->
    nondesc(N, 9).

nondesc(0, _) -> true;
nondesc(N, L) ->
  D = N rem 10,
  case D > L of
    true  -> false;
    false -> nondesc(N div 10, D)
  end.

repeated(N) ->
  repeated(N div 10, N rem 10).

repeated(0, _) -> false;
repeated(N, L) ->
  D = N rem 10,
  case D =:= L of
    true  -> true;
    false -> repeated(N div 10, D)
  end.

double(N) ->
  double(N div 10, N rem 10, 1).

double(0, _, 2) -> true;
double(0, _, _) -> false;
double(N, L, Run) ->
  D = N rem 10,
  NN = N div 10,
  case {D =:= L, Run} of
    { true, _} -> double(NN, D, Run + 1);
    {false, 2} -> true;
    {false, _} -> double(NN, D, 1)
  end.
