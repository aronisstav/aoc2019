#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/18

main(Args) ->
  Input = read_list("~s"),
  Map = make_map(Input),
  Sol =
    case Args of
      ["2"] -> 42;
      _ -> shortest(Map)
    end,
  io:format("~p~n", [Sol]).

%%------------------------------------------------------------------------------

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

%%------------------------------------------------------------------------------

make_map(Input) ->
  Map =
    #{ grid => #{}
     , keys => []
     , pos => {0, 0}
     },
  make_map(Input, 0, Map).

make_map([], _, Map) ->
  Map;
make_map([[Line]|Rest], Y, Map) ->
  NM = add_line(Line, 0, Y, Map),
  make_map(Rest, Y + 1, NM).

add_line([], _, _, Map) -> Map;
add_line([C|R], X, Y, Map) ->
  #{ grid := Grid
   , keys := Keys
   , pos := Pos
   } = Map,
  V = case C =:= $@ of true -> $.; false -> C end,
  NG = Grid#{{X,Y} => V},
  NK =
    case C >= $a andalso C =< $z of
      true -> [C|Keys];
      false -> Keys
    end,
  NP =
    case C =:= $@ of
      true -> {X, Y};
      false -> Pos
    end,
  NM =
    Map
    #{ grid => NG
     , keys => NK
     , pos => NP
     },
  add_line(R, X + 1, Y, NM).

%%------------------------------------------------------------------------------

shortest(Map) ->
  P = self(),
  ets:new(visited, [public, named_table]),
  NM =
    Map
    #{ hand => ordsets:new()
     , p => P
     , steps => 0
     },
  spawn(fun() -> child(NM) end),
  collect(0, 0, infinity).

collect(0, Max, Min) when is_number(Min) -> {Min, Max};
collect(N, Max, Min) ->
  receive
    new -> collect(N + 1, max(N + 1, Max), Min);
    dead -> collect(N - 1, Max, Min);
    {done, M} -> collect(N - 1, Max, min(M, Min))
  end.

child(Map) ->
  #{ p := P
   } = Map,
  link(P),
  P ! new,
  enter(Map).

enter(Map) ->
  #{ grid := Grid
   , hand := Hand
   , keys := Keys
   , p := P
   , pos := Pos
   , steps := N
   } = Map,
  ets:insert(visited, {{Pos, Hand}, N}),
  C = maps:get(Pos, Grid),
  {NG, NK, NH} =
    case C of
      C when C >= $A andalso C =< $Z ->
        K = C - $A + $a,
        true = ordsets:is_element(K, Hand),
        {Grid#{Pos => $.}, Keys, Hand};
      C when C >= $a andalso C =< $z ->
        {Grid#{Pos => $.}, Keys -- [C], ordsets:add_element(C,Hand)};
      $. ->
        {Grid, Keys, Hand}
    end,
  case NK =:= [] of
    true ->
      P ! {done, N};
    false ->
      NS = N + 1,
      NM =
        Map
        #{ grid => NG
         , hand => NH
         , keys => NK
         , steps => NS
         },
      case neighs(NM) of
        [] -> P ! dead;
        [Ne|Ighs] ->
          [spawn(fun() -> child(NM#{pos => I}) end) || I <- Ighs],
          enter(NM#{pos => Ne})
      end
  end.

neighs(Map) ->
  #{ grid := Grid
   , hand := Hand
   , pos := {X, Y}
   , steps := S
   } = Map,
  AllNeighs = [{X + 1, Y}, {X, Y + 1}, {X - 1, Y}, {X, Y - 1}],
  Filter =
    fun(Pos) ->
      C = maps:get(Pos, Grid),
      case C =:= $# of
        true -> false;
        false ->
          Visited = ets:lookup(visited, {Pos, Hand}),
          case Visited of
            [{_, NS}] when NS < S -> false;
            _ ->
              case C of
                $. -> true;
                C when C >= $a andalso C =< $z -> true;
                C when C >= $A andalso C =< $Z ->
                  K = C - $A + $a,
                  lists:member(K, Hand)
              end
          end
      end
    end,
  lists:filter(Filter, AllNeighs).
