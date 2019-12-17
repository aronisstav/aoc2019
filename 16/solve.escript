#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2019/day/16

main([Part|_]) ->
  [Input] = lists:append(read_list("~s")),
  Nums = [C - $0 || C <- Input],
  Sol =
    case Part of
      "1" -> fft(Nums);
      "2" -> "-1" %% full_fft(Nums)
    end,
  io:format("~s~n", [[S + $0 || S <- Sol]]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

fft(Nums) ->
  fft(1, Nums).

fft(101, Nums) -> Nums;
fft(N, Nums) ->
  New = fft_round(Nums),
  fft(N + 1, New).

fft_round(Nums) ->
  fft_round(Nums, 1, Nums, []).

fft_round([], _, _, Acc) ->
  lists:reverse(Acc);
fft_round([_|R], N, Nums, Acc) ->
  [_|First] = Pat = lists:append([lists:duplicate(N, I) || I <- [0,1,0,-1]]),
  D = fft_one(Nums, First, Pat, 0),
  fft_round(R, N + 1, Nums, [D|Acc]).

fft_one([], _, _, D) ->
  abs(D rem 10);
fft_one(Nums, [], Pat, D) ->
  fft_one(Nums, Pat, Pat, D);
fft_one([N|Ums], [M|Ask], Pat, D) ->
  fft_one(Ums, Ask, Pat, N * M + D).

%% full_fft(Nums) ->
%%   FNums = lists:append(lists:duplicate(10000, Nums)),
%%   {NS,_} = lists:split(7, FNums),
%%   N = list_to_integer([$0 + I || I <- NS]),
%%   Is = [N + I || I <- lists:seq(0, 7)],
%%   local_fft(FNums, Is).

%% local_fft(Nums, Indices) ->
%%   io:format("L: ~p~n", [length(Nums)]),
%%   Masks = masks(Indices, length(Nums)),
%%   io:format("Masks: ~p~n", [Masks]),
%%   Digits = digits(Nums, Masks),
%%   Final = local_fft(1, Digits, Masks),
%%   %io:format("~p~n", [Final]),
%%   retrieve(Final, Indices).

%% masks(Indices, L) ->
%%   Q = queue:from_list(Indices),
%%   masks(Q, L, #{}, #{}).

%% masks(Q, L, Masks, Plans) ->
%%   {Out, NQ} = queue:out(Q),
%%   case Out of
%%     empty -> Masks;
%%     {value, V} ->
%%       io:format("~p~n", [{V, L}]),
%%       Mask = find_mask(V, L),
%%       Fold = fun({I, K, _}, Acc) -> add_indices(I, K, Acc) end,
%%       {FQ, NPlans} = lists:foldl(Fold, {NQ, Plans}, Mask),
%%       masks(FQ, L, Masks#{V => Mask}, NPlans)
%%   end.

%% find_mask(I, L) ->
%%   find_mask(I, 1, I, L, []).

%% find_mask(N, _, _, L, Mask) when N > L -> lists:reverse(Mask);
%% find_mask(N, M, I, L, Mask) ->
%%   NN = N + I,
%%   MMask = [{N, NN, M}|Mask],
%%   find_mask(NN + I, M * -1, I, L, MMask).

%% add_indices(I, K, Acc) when I > K -> Acc;
%% add_indices(I, K, {Q, Plans} = Acc) ->
%%   NAcc =
%%     case maps:get(I, Plans, false) of
%%       true -> Acc;
%%       false -> {queue:in(I, Q), Plans#{I => true}}
%%     end,
%%   add_indices(I + 1, K, NAcc).

%% digits(Nums, Masks) ->
%%   Map = from_list(Nums),
%%   Fun = fun (K, _, Acc) -> Acc#{K => maps:get(K, Map)} end,
%%   maps:fold(Fun, #{}, Masks).

%% from_list(Nums) ->
%%   from_list(Nums, 1, #{}).

%% from_list([], _, Acc) -> Acc;
%% from_list([N|Ums], I, Acc) ->
%%   %% io:format("~p~n", [?LINE]),
%%   from_list(Ums, I + 1, Acc#{I => N}).

%% local_fft(101, D, _) -> D;
%% local_fft(N, D, M) ->
%%   Fun = fun (K, V, Acc) -> Acc#{K => apply_mask(V, D)} end,
%%   ND = maps:fold(Fun, #{}, M),
%%   io:format("~w~n", [{?LINE, ND}]),
%%   local_fft(N + 1, ND, M).

%% apply_mask(V, D) ->
%%   Fold = fun({I, K, M}, Acc) -> apply_mask(I, K, M, D, 0) end,
%%   %% io:format("~p~n", [?LINE]),
%%   abs(lists:foldl(Fold, 0, V) rem 10).

%% apply_mask(I, K, _, _, Acc) when I > K -> Acc;
%% apply_mask(I, K, M, D, Acc) ->
%%   NAcc = Acc + M * maps:get(I, D),
%%   apply_mask(I + 1, K, M, D, NAcc).

%% retrieve(D, Indices) ->
%%   [maps:get(I, D) || I <- Indices].
