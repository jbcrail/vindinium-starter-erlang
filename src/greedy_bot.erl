-module(greedy_bot).
-behaviour(vindinium_bot).

-export([move/1]).

move(State) ->
    vindinium_board:nearest_unowned_mine(State).
