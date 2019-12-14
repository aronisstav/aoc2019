#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/13

%% Run with the Intcode program as input

%%------------------------------------------------------------------------------

%-define(debug(Format, Args), io:format(Format, Args)).
-define(debug(_A, _B), ok).

-define(cmp(A), if A -> true; true -> false end).
-define(cmp(A, B, C), if A -> B; true -> C end).

%%------------------------------------------------------------------------------

main([Input]) ->
  Code = read_memory(Input),
  Grid = spawn_link(fun() -> grid() end),
  Robot = spawn_link(fun() -> robot(Code) end),
  Mon = monitor(process, Robot),
  Robot ! {grid, Grid},
  Grid ! {robot, Robot},
  receive
    {'DOWN', Mon, process, Robot, normal} -> ok
  end,
  Grid ! {stop, self()},
  receive
    N -> io:format("~p~n", [N])
  end.

grid() ->
  receive
    {robot, R} ->
      grid(#{grid => #{}, i => 0, mx => 10, my => 24, robot => R})
  end.

grid(State) ->
  case receive_command() of
    input ->
       #{robot := R, i := I} = State,
       R ! I,
       grid(State);
    {show_score, S} ->
       show(State),
       io:format("SCORE: ~p~n", [S]),
       grid(State);
    {stop, P} ->
       show(State),
       P ! count(2, State);
    {tile, Tile} ->
       NewState = update_grid(Tile, State),
       FinalState = maybe_move_paddle(NewState),
       grid(FinalState)
  end.

receive_command() ->
  receive
    X when is_number(X) ->
      Y = receive A when is_number(A) -> A end,
      I = receive B when is_number(B) -> B end,
      case X =:= -1 of
        true  -> {show_score, I};
        false -> {tile, {X, Y, I}}
      end;
    Y -> Y
  end.

count(C, #{grid := Grid}) ->
  Fun =
    fun(_, V, N) ->
      case V =:= C of
        true -> N + 1;
        false -> N
      end
    end,
  maps:fold(Fun, 0, Grid).

update_grid({X, Y, Id}, State) ->
  #{ grid := Grid
   , mx := MX
   , my := MY
   } = State,
  NewGrid = Grid#{{X, Y} => Id},
  NMX = scale(X, MX),
  NMY = scale(Y, MY),
  NewState = State#{grid => NewGrid, mx => NMX, my => NMY},
  case Id of
    3 -> NewState#{p => {X, Y}};
    4 -> NewState#{b => {X, Y}};
    _ -> NewState
  end.

scale(X, MX) ->
  case X > MX of
    true -> ((X div 10) + 1) * 10;
    false -> MX
  end.

maybe_move_paddle(#{p := {PX, PY}, b := {BX, BY}} = State) ->
  NI =
    if PX < BX -> 1;
       PX > BX -> -1;
       true -> 0
    end,
  State#{i => NI};
maybe_move_paddle(State) -> State.

%%------------------------------------------------------------------------------

show(State) ->
  #{ grid := Grid
   , mx := MX
   , my := MY
   } = State,
  show({0, MY}, {0, MX}, Grid).

show({MinY, MaxY}, {MinX, MaxX}, Grid) ->
  Ys = lists:seq(MinY, MaxY),
  Xs = lists:seq(MinX, MaxX),
  show_lines(Ys, Xs, Xs, Grid).

show_lines([], _, _, _) -> ok;
show_lines([_|RYs], [], Xs, Grid) ->
  io:format("~n"),
  show_lines(RYs, Xs, Xs, Grid);
show_lines([Y|RYs], [X|RXs], Xs, Grid) ->
  C =
    case maps:get({X, Y}, Grid, 0) of
      0 -> $ ;
      1 -> $#;
      2 -> $~;
      3 -> $_;
      4 -> $o
    end,
  io:format("~c",[C]),
  show_lines([Y|RYs], RXs, Xs, Grid).

%%------------------------------------------------------------------------------

robot(Code) ->
  receive
    {grid, G} ->
      NewCode =
        Code
        #{ input => fun() -> G ! input, receive N -> N end end
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
