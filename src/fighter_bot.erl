-module(fighter_bot).
-behaviour(vindinium_bot).

-export([move/1]).

move(State) ->
    vindinium_board:nearest_opponent(State).
