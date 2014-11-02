-module(greedy_bot).
-behaviour(vindinium_bot).

-export([move/1]).

move(State) ->
    Life = vindinium_state:life(State),
    if
        Life =< 25 ->
          vindinium_board:nearest_tavern(State);
        true ->
          Moves = [vindinium_board:nearest_unowned_mine(State), vindinium_board:nearest_unowned_mine(State)],
          lists:nth(random:uniform(length(Moves)), Moves)
    end.
