#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/15

%% Run as: ./solve.escript <intcode program> <part> ["step"]

%%------------------------------------------------------------------------------

%-define(debug(Format, Args), io:format(Format, Args)).
-define(debug(_A, _B), ok).

-define(cmp(A), if A -> true; true -> false end).
-define(cmp(A, B, C), if A -> B; true -> C end).

%%------------------------------------------------------------------------------

main([Input, Part|Rest]) ->
  Code = read_memory(Input),
  Self = self(),
  Grid = spawn_link(fun() -> grid_i(Self, Part, Rest) end),
  Robot = spawn_link(fun() -> robot(Code) end),
  Robot ! {grid, Grid},
  Grid ! {robot, Robot},
  receive
    {least, N} -> io:format("~p~n", [N])
  end,
  Robot ! stop.

grid_i(P, Part, Rest) ->
  receive
    {robot, R} ->
      State =
        #{ dir => $^
         , found => false
         , grid => #{{0,0} => 1}
         , parent => P
         , part => Part
         , pos => {0, 0}
         , rest => Rest
         , robot => R
         },
      grid(State)
  end.

grid(State) ->
  pretty(State),
  {Command, NewState} = decide(State),
  case Command of
    {move, D} -> grid(move(D, NewState));
    found ->
      N = bfs(State),
      #{parent := P} = State,
      P ! {least, N}
  end.

pretty(State) ->
  #{rest := Rest} = State,
  case lists:member("step", Rest) of
    true ->
      io:format(os:cmd("clear")),
      show(State);
    false ->
      ok
  end.

decide(#{found := F, pos := {0, 0}} = State) when F =/= false ->
  show(State),
  {found, State};
decide(State) ->
  #{ dir := Dir
   , grid := Grid
   , pos := Pos
   } = State,
  Right = right(Pos, Dir),
  case maps:get(Right, Grid, -1) of
    0 ->
      Front = front(Pos, Dir),
      case maps:get(Front, Grid, -1) of
        0 -> decide(State#{dir => left(Dir)});
        _ -> {{move, Dir}, State}
      end;
    _ -> {{move, right(Dir)}, State}
  end.

front({X, Y}, D) ->
  case D of
    $^ -> {X, Y + 1};
    $v -> {X, Y - 1};
    $< -> {X - 1, Y};
    $> -> {X + 1, Y}
  end.

right(D) ->
  case D of
    $^ -> $>;
    $v -> $<;
    $< -> $^;
    $> -> $v
  end.

left(D) ->
  case D of
    $v -> $>;
    $^ -> $<;
    $> -> $^;
    $< -> $v
  end.

right(Pos, D) ->
  front(Pos, right(D)).

move(C, State) ->
  #{ dir := Dir
   , found := Found
   , grid := Grid
   , pos := {X, Y} = Pos
   , robot := R
   } = State,
  {D, T} =
    case C of
      $^ -> {1, {X, Y + 1}};
      $v -> {2, {X, Y - 1}};
      $< -> {3, {X - 1, Y}};
      $> -> {4, {X + 1, Y}}
    end,
  R ! D,
  {NewPos, NewGrid, NewFound} =
    receive
      0 ->
        {Pos, Grid#{T => 0}, Found};
      1 ->
        {T, Grid#{T => 1}, Found};
      2 ->
        {T, Grid#{T => 2}, T}
    end,
  NewDir =
    case NewPos =/= Pos of
      true -> C;
      false -> Dir
    end,
  State
    #{ dir => NewDir
     , found => NewFound
     , grid => NewGrid
     , pos => NewPos
     }.

bfs(State) ->
  #{ found := Found
   , grid := Grid
   , part := Part
   } = State,
  From =
    case Part of
      "1" -> {0, 0};
      "2" -> Found
    end,
  bfs(queue:from_list([{0, From}]), #{max => 0, part => Part}, Grid).

bfs(InQueue, Vis, Grid) ->
  {Out, Queue} = queue:out(InQueue),
  case Out of
    {value, V} ->
      bfs(V, Queue, Vis, Grid);
    empty ->
      #{max := Max} = Vis,
      Max
  end.

bfs({N, {X, Y} = Pos}, Queue, #{max := Max, part := Part} = Vis, Grid) ->
  case maps:get(Pos, Vis, false) of
    true -> bfs(Queue, Vis, Grid);
    false ->
      case maps:get(Pos, Grid, 0) of
        2 when Part =:= "1" -> N;
        0 -> bfs(Queue, Vis, Grid);
        _ ->
          NewVis = Vis#{Pos => true, max => max(N, Max)},
          Near = [{X, Y - 1},{X - 1, Y},{X, Y + 1}, {X + 1, Y}],
          Fold = fun(I, A) -> queue:in({N + 1, I}, A) end,
          NewQueue = lists:foldl(Fold, Queue, Near),
          bfs(NewQueue, NewVis, Grid)
      end
  end.

%%------------------------------------------------------------------------------

show(State) ->
  #{ dir := Dir
   , grid := Grid
   , pos := Pos
   } = State,
  Fun =
    fun({X, Y}, _, {MinX, MaxX, MinY, MaxY}) ->
        {min(MinX, X), max(MaxX, X), min(MinY, Y), max(MaxY, Y)}
    end,
  {MinX, MaxX, MinY, MaxY} = maps:fold(Fun, {0,0,0,0}, Grid),
  show({MinY, MaxY}, {MinX, MaxX}, Grid#{Pos => Dir}).

show({MinY, MaxY}, {MinX, MaxX}, Grid) ->
  Ys = lists:reverse(lists:seq(MinY, MaxY)),
  Xs = lists:seq(MinX, MaxX),
  show_lines(Ys, Xs, Xs, Grid).

show_lines([], _, _, _) -> ok;
show_lines([_|RYs], [], Xs, Grid) ->
  io:format("~n"),
  show_lines(RYs, Xs, Xs, Grid);
show_lines([Y|RYs], [X|RXs], Xs, Grid) ->
  C =
    case maps:get({X, Y}, Grid, -1) of
      -1 -> $.;
      0 -> $#;
      1 ->
        case {X, Y} =:= {0, 0} of
          true -> $$;
          false -> $ %
        end;
      2 -> $!;
      S -> S
    end,
  io:format("~c", [C]),
  show_lines([Y|RYs], RXs, Xs, Grid).

%%------------------------------------------------------------------------------

robot(Code) ->
  receive
    {grid, G} ->
      NewCode =
        Code
        #{ input => fun() -> receive N -> N end end
         , output => fun(O) -> G ! O end
         },
      execute(NewCode)
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
    99 -> io:format("halt~n"), Code
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
