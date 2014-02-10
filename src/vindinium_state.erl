-module(vindinium_state).

-export([from_json/1, finished/1, play_url/1]).

-record(state, {game, hero, token, view_url, play_url}).
-record(game, {id, turn, max_turns, heroes, board, finished}).
-record(hero, {id, name, user_id, elo, pos, life, gold, mine_count, spawn_pos, crashed}).
-record(board, {size, tiles}).
-record(position, {x, y}).

from_json(Json) ->
    kvc:to_proplist(mochijson2:decode(Json)).

finished(State) ->
    proplists:get_value(<<"finished">>, proplists:get_value(<<"game">>, State)).

play_url(State) ->
    binary_to_list(proplists:get_value(<<"playUrl">>, State)).
