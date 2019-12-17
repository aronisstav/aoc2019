#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/17

%% Run as: ./solve.escript <intcode program> <part>
%% Program for part 2 is hardcoded, after analysis by hand

%%------------------------------------------------------------------------------

%-define(debug(Format, Args), io:format(Format, Args)).
-define(debug(_A, _B), ok).

-define(cmp(A), if A -> true; true -> false end).
-define(cmp(A, B, C), if A -> B; true -> C end).

%%------------------------------------------------------------------------------

main([Input, Part|_]) ->
  Code = read_memory(Input),
  Grid = spawn_link(fun() -> grid_i(Part) end),
  Robot = spawn_link(fun() -> robot(Code, Part) end),
  Mon = monitor(process, Robot),
  Grid ! {robot, Robot},
  Robot ! {grid, Grid},
  receive
    {'DOWN', Mon, process, Robot, normal} -> ok
  end,
  Grid ! {finish, self()},
  receive ok -> ok end.

grid_i(Part) ->
  receive
    {robot, R} ->
      State =
        #{ grid => #{{0,0} => 1}
         , part => Part
         , pos => {0, 0}
         , robot => R
         },
      case Part of
        "1" -> ok;
        "2" ->
          Program =
            "A,B,A,C,B,C,B,C,A,C\n"
            "R,12,L,6,R,12\n"
            "L,8,L,6,L,10\n"
            "R,12,L,10,L,6,R,10\n"
            "n\n",
          [R ! C || C <- Program],
          ok
      end,
      grid(State)
  end.

grid(#{part := "1"} = State) ->
  #{ grid := Grid
   , pos := {X, Y} = Pos
   } = State,
  receive
    $\n ->
      NS = State#{pos := {0, Y + 1}},
      grid(NS);
    C when C < 256 ->
      NS = State#{pos := {X + 1, Y}, grid => Grid#{Pos => C}},
      grid(NS);
    {finish, P} ->
      show(State),
      N = alignment(State),
      io:format("~p~n", [N]),
      P ! ok
  end;
grid(#{part := "2"} = State) ->
  receive
    C when C < 256 ->
      io:format("~c", [C]),
      grid(State);
    C when is_integer(C) ->
      io:format("~p~n", [C]),
      grid(State);
    {finish, P} ->
      P ! ok
  end.

%%------------------------------------------------------------------------------

show(State) ->
  #{ grid := Grid
   } = State,
  Fun =
    fun({X, Y}, _, {MinX, MaxX, MinY, MaxY}) ->
        {min(MinX, X), max(MaxX, X), min(MinY, Y), max(MaxY, Y)}
    end,
  {MinX, MaxX, MinY, MaxY} = maps:fold(Fun, {0, 0, 0, 0}, Grid),
  show({MinY, MaxY}, {MinX, MaxX}, Grid).

show({MinY, MaxY}, {MinX, MaxX}, Grid) ->
  Ys = lists:seq(MinY, MaxY),
  Xs = lists:seq(MinX, MaxX),
  show_lines(Ys, Xs, Xs, Grid).

show_lines([], _, _, _) -> ok;
show_lines([_|RYs], [], Xs, Grid) ->
  io:format("~n"),
  show_lines(RYs, Xs, Xs, Grid);
show_lines([Y|RYs], [X|RXs], Xs, Grid) ->
  C = maps:get({X, Y}, Grid),
  io:format("~c", [C]),
  show_lines([Y|RYs], RXs, Xs, Grid).

%%------------------------------------------------------------------------------

alignment(State) ->
  #{ grid := Grid
   } = State,
  Fold =
    fun({X, Y}, $#, I) ->
        N = get({X, Y + 1}, Grid),
        E = get({X + 1, Y}, Grid),
        S = get({X, Y - 1}, Grid),
        W = get({X - 1, Y}, Grid),
        case {N, E, S, W} of
          {$#, $#, $#, $#} -> I + X * Y;
          _ -> I
        end;
       (_, _, I) -> I
    end,
  maps:fold(Fold, 0, Grid).

get(Pos, Grid) ->
  maps:get(Pos, Grid, $.).
%%------------------------------------------------------------------------------

robot(Code, Part) ->
  receive
    {grid, G} ->
      NewCode =
        Code
        #{ input => fun() -> receive N -> N end end
         , output => fun(O) -> G ! O end
         },
      case Part of
        "1" -> execute(NewCode);
        "2" -> execute(NewCode#{0 => 2})
      end
  end.

%%------------------------------------------------------------------------------

read_memory(File) ->
  {ok, Bin} = file:read_file(File),
  Str = binary_to_list(Bin),
  Tokens = string:tokens(Str, ","),
  read_memory(Tokens, 0, #{}).

read_memory([], _, Acc) -> Acc;
read_memory([T|R], I, Acc) ->
  NAcc = Acc#{I => list_to_integer(T)},
  read_memory(R, I + 1, NAcc).

%%------------------------------------------------------------------------------

execute(Code) ->
  execute(0, Code#{rb => 0}).

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
    09 -> adjust_rb(P, Code);
    99 -> ok
  end.

%%------------------------------------------------------------------------------

three_op(P, Fun, Code) ->
  P1 = P + 1,
  P2 = P + 2,
  P3 = P + 3,
  #{P := Op, P1 := Op1, P2 := Op2, P3 := Op3} = Code,
  ?debug("%% ~p ~p ~p ~p: ", [Op, Op1, Op2, Op3]),
  V1 = get_v(Op, 100, Op1, Code),
  V2 = get_v(Op, 1000, Op2, Code),
  R = Fun(V1, V2),
  Addr = get_addr(Op, 10000, Op3, Code),
  ?debug("~p ~p -> ~p~n", [V1, V2, R]),
  NewCode = Code#{Addr => R},
  execute(P + 4, NewCode).

input(P, Code) ->
  P1 = P + 1,
  #{P := Op, P1 := Op1, input := Input} = Code,
  D = Input(),
  Addr = get_addr(Op, 100, Op1, Code),
  NewCode = Code#{Addr => D},
  execute(P + 2, NewCode).

output(P, Code) ->
  P1 = P + 1,
  #{P := Op, P1 := Op1, output := Output} = Code,
  V1 = get_v(Op, 100, Op1, Code),
  Output(V1),
  execute(P + 2, Code).

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

adjust_rb(P, Code) ->
  P1 = P + 1,
  #{P := Op, P1 := Op1, rb := RB} = Code,
  ?debug("%% ~p ~p: ", [Op, Op1]),
  V1 = get_v(Op, 100, Op1, Code),
  R = RB + V1,
  ?debug("rb ~p ~p -> ~p~n", [RB, V1, R]),
  NewCode = Code#{rb => R},
  execute(P + 2, NewCode).

%%------------------------------------------------------------------------------

get_v(Op, Div, Arg, #{rb := RB} = Code) ->
  case (Op div Div) rem 10 of
    0 -> maps:get(Arg, Code, 0);
    1 -> Arg;
    2 -> maps:get(RB + Arg, Code, 0)
  end.

get_addr(Op, Div, Arg, #{rb := RB}) ->
  case (Op div Div) rem 10 of
    0 -> Arg;
    2 -> RB + Arg
  end.
