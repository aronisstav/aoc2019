#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/10

%% Part 2: call with "2 index", assumed to be < answer for part 1.

main(Args) ->
  Input = read_list("~s"),
  Asteroids = asteroid_map(Input),
  Sights = find_sights(Asteroids),
  Fun = fun(K, {V, L}, Acc) -> max({V, K, L}, Acc) end,
  {Max, T, L} = maps:fold(Fun, {0, {0,0}, []}, Sights),
  Sol =
    case Args of
      ["2", I] ->
        LL = [order(C, T) || C <- L],
        ByAngle = lists:sort(LL),
        {_, {X, Y}} = lists:nth(list_to_integer(I), ByAngle),
        X * 100 + Y;
      _ -> Max
    end,
  io:format("~p~n", [Sol]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

asteroid_map(Input) ->
  asteroid_map(Input, 0, #{}).

asteroid_map([], _, Map) -> Map;
asteroid_map([[Line]|Rest], Y, Map) ->
  NewMap = asteroid_line(Line, 0, Y, Map),
  asteroid_map(Rest, Y + 1, NewMap).

asteroid_line([], _, _, Map) -> Map;
asteroid_line([C|R], X, Y, Map) ->
  NewMap =
    case C of
      $# -> Map#{{X, Y} => $#};
      $. -> Map
    end,
  asteroid_line(R, X + 1, Y, NewMap).

find_sights(Asteroids) ->
  Fun =
    fun(K, _, Acc) ->
      S = find_sight(K, Asteroids),
      Acc#{K => S}
    end,
  maps:fold(Fun, #{}, Asteroids).

find_sight({AX, AY}, Asteroids) ->
  Fun =
    fun({CX, CY}, _, Acc) when AX =:= CX, AY =:= CY -> Acc;
       ({BX, BY}, _, {N, L}) ->
      {DX, DY} = {BX - AX, BY - AY},
      {M, {UX, UY}} = unit_scale(DX, DY),
      Obscurers = [{AX + I * UX, AY + I * UY} || I <- lists:seq(1, M - 1)],
      Fun = fun(C) -> maps:get(C, Asteroids, false) =:= false end,
      case lists:all(Fun, Obscurers) of
        true -> {N + 1, [{BX, BY}|L]};
        false -> {N, L}
      end
    end,
  maps:fold(Fun, {0, []}, Asteroids).

unit_scale(X, Y) ->
  GCD = gcd(abs(X), abs(Y)),
  {GCD, {X div GCD, Y div GCD}}.

gcd(0, 0) -> 1;
gcd(A, 0) -> A;
gcd(A, B) when B > A -> gcd(B, A);
gcd(A, B) when A rem B > 0 -> gcd(B, A rem B);
gcd(A, B) when A rem B =:= 0 -> B.

order({XB, YB}, {XA, YA}) ->
  {DX, DY} = {YA - YB, XB - XA},
  ATan = math:atan2(DY, DX),
  Ord =
    case ATan < 0 of
      true -> ATan + 2 * math:pi();
      false -> ATan
    end,
  {Ord, {XB, YB}}.
