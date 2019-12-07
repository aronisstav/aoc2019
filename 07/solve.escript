#!/usr/bin/env escript
%-mode(native).

%% https://adventofcode.com/2019/day/7

%% Run with part and the Intcode program file as input

%-define(debug(Format, Args), io:format(Format, Args)).
-define(debug(_A, _B), ok).

-define(cmp(A), if A -> true; true -> false end).
-define(cmp(A, B, C), if A -> B; true -> C end).

main([Mode,Input]) ->
  Code = read_memory(Input),
  case Mode of
    "1" ->
      Perms = permutations(lists:seq(0,4)),
      Max = find_max(Perms, Code),
      io:format("~p~n", [Max]);
    "2" ->
      Perms = permutations(lists:seq(5,9)),
      Max = find_loopi_max(Perms, Code),
      io:format("~p~n", [Max])
  end.

read_memory(File) ->
  {ok, Bin} = file:read_file(File),
  Str = binary_to_list(Bin),
  Tokens = string:tokens(Str, ","),
  read_memory(Tokens, 0, #{}).

read_memory([], _, Acc) -> Acc;
read_memory([T|R], I, Acc) ->
  NAcc = Acc#{I => list_to_integer(T)},
  read_memory(R, I + 1, NAcc).

permutations([]) -> [[]];
permutations(L) ->
  [[H|T]|| H <- L, T <- permutations(L -- [H])].

find_max(Perms, Code) ->
  put(next, [self()]),
  find_max(Perms, Code, 0).

find_max([], _, Max) -> Max;
find_max([Perm|Rest], Code, Max) ->
  self() ! 0,
  run_all(Perm, Code),
  receive
    I -> find_max(Rest, Code, max(I, Max))
  end.

run_all([], _) -> ok;
run_all([I|Rest], Code) ->
  receive
    O ->
      self() ! I,
      self() ! O,
      execute(Code)
  end,
  run_all(Rest, Code).

find_loopi_max(Perms, Code) ->
  find_loopi_max(Perms, Code, 0).

find_loopi_max([], _, Max) -> Max;
find_loopi_max([Perm|Rest], Code, Max) ->
  I = run_loopi(Perm, Code),
  find_loopi_max(Rest, Code, max(I, Max)).

run_loopi(Perm, Code) ->
  [S1, S2, S3, S4, S5] = Perm,
  Parent = self(),
  [P1, P2, P3, P4, P5] = [spawn(fun() -> child(Code) end) || _ <- lists:seq(1, 5)],
  M = monitor(process, P5),
  P1 ! [P2],
  P2 ! [P3],
  P3 ! [P4],
  P4 ! [P5],
  P5 ! [P1, Parent],
  P1 ! S1,
  P2 ! S2,
  P3 ! S3,
  P4 ! S4,
  P5 ! S5,
  P1 ! 0,
  collect(M, P5).

child(Code) ->
  receive
    N ->
      put(next, N),
      execute(Code)
  end.

collect(M, C) ->
  collect(M, C, 0).

collect(M, C, O) ->
  receive
    {'DOWN', M, process, C, normal} -> O;
    N -> collect(M, C, N)
  end.

execute(Code) ->
  execute(0, Code).

execute(P, Code) ->
  #{P := Op} = Code,
  case Op rem 100 of
    01 -> three_op(P, fun(X, Y) -> X + Y end, Code);
    02 -> three_op(P, fun(X, Y) -> X * Y end, Code);
    03 -> input(P, Code);
    04 -> output(P, Code);
    05 -> jump(P, fun(X) -> ?cmp(X =/= 0) end, Code);
    06 -> jump(P, fun(X) -> ?cmp(X =:= 0) end, Code);
    07 -> three_op(P, fun(X, Y) -> ?cmp(X  < Y, 1, 0) end, Code);
    08 -> three_op(P, fun(X, Y) -> ?cmp(X == Y, 1, 0) end, Code);
    99 -> Code
  end.

three_op(P, Fun, Code) ->
  P1 = P + 1,
  P2 = P + 2,
  P3 = P + 3,
  #{P := Op, P1 := Op1, P2 := Op2, P3 := Op3} = Code,
  ?debug("%% ~p ~p ~p ~p: ", [Op, Op1, Op2, Op3]),
  V1 = get_v(Op, 100, Op1, Code),
  V2 = get_v(Op, 1000, Op2, Code),
  R = Fun(V1, V2),
  ?debug("~p ~p -> ~p~n", [V1, V2, R]),
  NewCode = Code#{Op3 => R},
  execute(P + 4, NewCode).

input(P, Code) ->
  P1 = P + 1,
  #{P1 := Op1} = Code,
  D = receive I -> I end,
  NewCode = Code#{Op1 => D},
  execute(P + 2, NewCode).

output(P, Code) ->
  P1 = P + 1,
  #{P := Op, P1 := Op1} = Code,
  V1 = get_v(Op, 100, Op1, Code),
  [T ! V1 || T <- get(next)],
  execute(P + 2, Code).

get_v(Op, Div, Arg, Code) ->
  case (Op div Div) rem 10 =/= 0 of
    true -> Arg;
    false -> maps:get(Arg, Code)
  end.

jump(P, Fun, Code) ->
  P1 = P + 1,
  P2 = P + 2,
  #{P := Op, P1 := Op1, P2 := Op2} = Code,
  ?debug("%% ~p ~p ~p: ", [Op, Op1, Op2]),
  V1 = get_v(Op, 100, Op1, Code),
  V2 = get_v(Op, 1000, Op2, Code),
  R = Fun(V1),
  ?debug("~p ~p -> ~p~n", [V1, V2, R]),
  case R of
    true -> execute(V2, Code);
    false -> execute(P + 3, Code)
  end.
