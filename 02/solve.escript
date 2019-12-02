#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/2

%% Part 1 must be called as "1 <Value for 1> <Value for 2>", given in description
%% Part 2 must be calles as "2 <Target value>", given in description

main(Args) ->
  Code = read_memory(),
  Sol =
    case Args of
      ["2", TargetStr] ->
        Target = list_to_integer(TargetStr),
        find_target(Target, Code);
      ["1" | NVStr] ->
        [Noun, Verb] = [list_to_integer(S) || S <- NVStr],
        Fix = Code#{1 => Noun, 2 => Verb},
        #{0 := V} = execute(Fix),
        V
    end,
  io:format("~w~n", [Sol]).

read_memory() ->
  {ok, [Str]} = io:fread("", "~s"),
  Tokens = string:tokens(Str, ","),
  read_memory(Tokens, 0, #{}).

read_memory([], _, Acc) -> Acc;
read_memory([T|R], I, Acc) -> 
  NAcc = Acc#{I => list_to_integer(T)},
  read_memory(R, I + 1, NAcc).

execute(Code) ->
  execute(0, Code).

execute(P, Code) ->
  #{P := Op} = Code,
  case Op of
    01 -> three_op(P, fun(X, Y) -> X + Y end, Code);
    02 -> three_op(P, fun(X, Y) -> X * Y end, Code);
    99 -> Code
  end.

three_op(P, Fun, Code) ->
  P1 = P + 1,
  P2 = P + 2,
  P3 = P + 3,
  #{P1 := Op1, P2 := Op2, P3 := Op3} = Code,
  #{Op1 := V1, Op2 := V2} = Code,
  NewCode = Code#{Op3 => Fun(V1,V2)},
  execute(P + 4, NewCode).

find_target(Target, Code) ->
  Nums = lists:seq(0, 99),
  NounVerbs = [{N, V} || N <- Nums, V <- Nums],
  find_target(NounVerbs, Code, Target).

find_target([], _Code, _Target) ->
  -1;
find_target([{N, V}|Rest], Code, Target) ->
  Fix = Code#{1 => N, 2 => V},
  #{0 := R} = execute(Fix),
  case R =:= Target of
    true -> N * 100 + V;
    false-> find_target(Rest, Code, Target)
  end.
