#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/1

main(Args) ->
  Nums = lists:append(read_list("~d")),
  Sol =
    case Args of
      ["2"] -> fuel_proper(Nums);
      _ -> fuel_mod(Nums)
    end,
  io:format("~w~n", [Sol]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

fuel_mod(Nums) ->
  lists:sum([(Mod div 3) - 2 || Mod <- Nums]).

fuel_proper(Nums) ->
  lists:sum([mod_proper(Mod) || Mod <- Nums]).

mod_proper(Mod) ->
  mod_proper(Mod, 0).

mod_proper(Lift, Fuel) ->
  NF = (Lift div 3) - 2,
  case NF < 0 of
    true -> Fuel;
    false -> mod_proper(NF, Fuel + NF)
  end.
