#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/12

main(Args) ->
  [Part, Steps] = [list_to_integer(N) || N <- Args],
  Input = read_list("<x=~d, y=~d, z=~d>"),
  Init = [#{pos => {X, Y, Z}, v => {0, 0, 0}} || [X, Y, Z] <- Input],
  Sol =
    case Part of
      1 -> energy(Init, Steps);
      2 -> repeat(Init)
    end,
  io:format("~w~n", [Sol]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

energy(Init, Steps) ->
  Final = simulate(Init, Steps),
  energy_sum(Final).

simulate(Moons, 0) -> Moons;
simulate(Moons, N) ->
  Gravity = apply_gravity(Moons),
  Velocity = [apply_velocity(M) || M <- Gravity],
  simulate(Velocity, N - 1).

apply_gravity(Moons) ->
  [apply_gravity(Moon, Moons -- [Moon]) || Moon <- Moons].

apply_gravity(Moon, []) -> Moon;
apply_gravity(Moon, [M|Rest]) ->
  #{pos := {X, Y, Z}, v := {VX, VY, VZ}} = Moon,
  #{pos := {XX, YY, ZZ}} = M,
  NVX = pull(X, XX, VX),
  NVY = pull(Y, YY, VY),
  NVZ = pull(Z, ZZ, VZ),
  apply_gravity(Moon#{v => {NVX, NVY, NVZ}}, Rest).

pull(I, I, V) -> V;
pull(I, J, V) when I < J -> V + 1;
pull(I, J, V) when I > J -> V - 1.

apply_velocity(Moon) ->
  #{pos := {X, Y, Z}, v := {VX, VY, VZ}} = Moon,
  Moon#{pos => {X + VX, Y + VY, Z + VZ}}.

energy_sum(Moons) ->
  lists:sum([energy(M) || M <- Moons]).

energy(Moon) ->
  #{pos := {X, Y, Z}, v := {VX, VY, VZ}} = Moon,
  (abs(X) + abs(Y) + abs(Z)) * (abs(VX) + abs(VY) + abs(VZ)).

repeat(Init) ->
  NX = period(Init, 1),
  NY = period(Init, 2),
  NZ = period(Init, 3),
  LCA = lcm(NX, NY),
  LCM = lcm(LCA, NZ),
  {{NX, NY, NZ}, LCM}.

period(Moons, Element) ->
  OneD =
    [{element(Element, Pos), element(Element, Vs)}
     || #{pos := Pos, v := Vs} <- Moons],
  period_1d(OneD, #{}, 0).

period_1d(OneD, Map, N) ->
  P = maps:get(OneD, Map, nope),
  case P =:= nope of
    true ->
      NewMap = Map#{OneD => N},
      NewMoons = simulate_1d(OneD),
      period_1d(NewMoons, NewMap, N + 1);
    false ->
      N - P
  end.

simulate_1d(OneD) ->
    Gravity = [gravity_1d(O, OneD -- [O]) || O <- OneD],
    [{D + VD, VD} || {D, VD} <- Gravity].

gravity_1d(O, []) -> O;
gravity_1d({D, VD}, [{O, _VO}|R]) ->
  NVD =
    if D =:= O -> VD;
       D  <  O -> VD + 1;
       D  >  O -> VD - 1
    end,
  gravity_1d({D, NVD}, R).

lcm(A, B) ->
    (A * B) div gcd(A, B).

gcd(0, 0) -> 1;
gcd(A, 0) -> A;
gcd(A, B) when B > A -> gcd(B, A);
gcd(A, B) when A rem B > 0 -> gcd(B, A rem B);
gcd(A, B) when A rem B =:= 0 -> B.
