#!/usr/bin/env escript
%-mode(native).

%% https://adventofcode.com/2019/day/14

main(Args) ->
  Input = read_list(),
  Recipes = make_recipes(Input),
  Sol =
    case Args of
      ["2"] -> make_most_fuel(Recipes);
      _ -> make_fuel(Recipes)
    end,
  io:format("~w~n", [Sol]).

read_list() ->
  read_list([]).

read_list(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    Res -> read_list([Res|Acc])
  end.

make_recipes(Input) ->
  make_recipes(Input, #{}).

make_recipes([], Recipes) -> Recipes;
make_recipes([Line|Rest], Recipes) ->
  Tokens = string:tokens(Line," ,\n"),
  {Res, Q, Reqs} = make_recipe(Tokens),
  no = maps:get(Res, Recipes, no),
  make_recipes(Rest, Recipes#{Res => {Q, Reqs}}).

make_recipe(Tokens) ->
  make_recipe(Tokens, #{}).

make_recipe(["=>", Num, Res], Reqs) ->
  {Res, list_to_integer(Num), Reqs};
make_recipe([Num, Req|Rest], Reqs) ->
  N = list_to_integer(Num),
  make_recipe(Rest, Reqs#{Req => N}).

make_fuel(Recipes) ->
  Target = "FUEL",
  {1, Reqs} = maps:get(Target, Recipes),
  {Ore, _} = make(Reqs, #{}, 0, Recipes),
  Ore.

make(#{"ORE" := N} = Need, Leftover, Ore, Recipes) ->
  NNeed = maps:remove("ORE", Need),
  make(NNeed, Leftover, Ore + N, Recipes);
make(OldNeed, Leftover, Ore, Recipes) ->
  I = maps:iterator(OldNeed),
  case maps:next(I) of
    none -> {Ore, Leftover};
    {NK, NV, _} ->
      Need = maps:remove(NK, OldNeed),
      LV = maps:get(NK, Leftover, 0),
      {NewNeed, NewLeftover} =
        case LV >= NV of
          true ->
            NNeed = maps:remove(NK, Need),
            NLO = Leftover#{NK => LV - NV},
            {NNeed, NLO};
          false ->
            FNV = NV - LV,
            {Q, Reqs} = maps:get(NK, Recipes),
            N = trunc(math:ceil(FNV / Q)),
            NLO = Leftover#{NK => N * Q - FNV},
            Fun =
              fun(RK, RV, NAcc) ->
                  ON = maps:get(RK, NAcc, 0),
                  NAcc#{RK => ON + RV * N}
              end,
            NNeed = maps:fold(Fun, Need, Reqs),
            {NNeed, NLO}
        end,
      make(NewNeed, NewLeftover, Ore, Recipes)
  end.

make_most_fuel(Recipes) ->
  make_most_fuel(1000000000000, #{}, 100000, 0, Recipes).

make_most_fuel(_, _, 0, Fu, _) -> Fu;
make_most_fuel(Left, OLO, Unit, Fu, Recipes) ->
  Target = "FUEL",
  {1, Reqs} = maps:get(Target, Recipes),
  Fun =
    fun(RK, RV, NAcc) ->
        NAcc#{RK => RV * Unit}
    end,
  Need = maps:fold(Fun, #{}, Reqs),
  {Ore, LO} = make(Need, OLO, 0, Recipes),
  case Left >= Ore of
    true ->
      NewLeft = Left - Ore,
      NewFu = Fu + Unit,
      make_most_fuel(NewLeft, LO, Unit, NewFu, Recipes);
    false ->
      make_most_fuel(Left, LO, Unit div 10, Fu, Recipes)
  end.
