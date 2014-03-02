-module(vindinium_bot).
-include("vindinium.hrl").

-export([random_direction/1]).

-callback move(_State) -> string().

random_direction(State) ->
    Valid = vindinium_board:valid_moves(State),
    lists:nth(random:uniform(length(Valid)), Valid).
