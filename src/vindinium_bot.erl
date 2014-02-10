-module(vindinium_bot).

-export([directions/0, random_direction/0]).

-callback move(_State) -> string().

directions() ->
    ["Stay", "North", "South", "East", "West"].

random_direction() ->
    Directions = directions(),
    lists:nth(random:uniform(length(Directions)), Directions).
