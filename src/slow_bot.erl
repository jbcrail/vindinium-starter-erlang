-module(slow_bot).
-behaviour(vindinium_bot).

-export([move/1]).

move(State) ->
    timer:sleep(1500),
    vindinium_bot:random_direction().
