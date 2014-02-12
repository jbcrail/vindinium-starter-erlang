-module(vindinium_state).

-export([from_json/1, finished/1, play_url/1, winner/1]).

-record(state, {game, hero, token, view_url, play_url}).
-record(game, {id, turn, max_turns, heroes, board, finished}).
-record(hero, {id, name, user_id, elo, pos, life, gold, mine_count, spawn_pos, crashed}).
-record(board, {size, tiles}).
-record(position, {x, y}).

to_board(Proplist) ->
    #board{
      size=proplists:get_value(<<"size">>, Proplist),
      tiles=proplists:get_value(<<"tiles">>, Proplist)
      }.

to_position(Proplist) ->
    #position{
      x=proplists:get_value(<<"x">>, Proplist),
      y=proplists:get_value(<<"y">>, Proplist)
      }.

to_hero(Proplist) ->
    #hero{
      id=proplists:get_value(<<"id">>, Proplist),
      name=proplists:get_value(<<"name">>, Proplist),
      user_id=proplists:get_value(<<"userId">>, Proplist),
      elo=proplists:get_value(<<"elo">>, Proplist),
      pos=to_position(proplists:get_value(<<"pos">>, Proplist)),
      life=proplists:get_value(<<"life">>, Proplist),
      gold=proplists:get_value(<<"gold">>, Proplist),
      mine_count=proplists:get_value(<<"mineCount">>, Proplist),
      spawn_pos=to_position(proplists:get_value(<<"spawnPos">>, Proplist)),
      crashed=proplists:get_value(<<"crashed">>, Proplist)
      }.

to_game(Proplist) ->
    #game{
      id=proplists:get_value(<<"id">>, Proplist),
      turn=proplists:get_value(<<"turn">>, Proplist),
      max_turns=proplists:get_value(<<"maxTurns">>, Proplist),
      heroes=lists:map(fun(P) -> to_hero(P) end, proplists:get_value(<<"heroes">>, Proplist)),
      board=to_board(proplists:get_value(<<"board">>, Proplist)),
      finished=proplists:get_value(<<"finished">>, Proplist)
      }.

to_state(Proplist) ->
    #state{
      game=to_game(proplists:get_value(<<"game">>, Proplist)),
      hero=to_hero(proplists:get_value(<<"hero">>, Proplist)),
      token=proplists:get_value(<<"token">>, Proplist),
      view_url=proplists:get_value(<<"viewUrl">>, Proplist),
      play_url=proplists:get_value(<<"playUrl">>, Proplist)
      }.

from_json(Json) ->
    to_state(kvc:to_proplist(mochijson2:decode(Json))).

finished(State) ->
    State#state.game#game.finished.

play_url(State) ->
    binary_to_list(State#state.play_url).

max_hero(Hero, Acc) when Acc =:= undefined ->
    Hero;
max_hero(Hero, Acc) when Hero#hero.gold > Acc#hero.gold ->
    Hero;
max_hero(_, Acc) ->
    Acc.

winner(State) ->
    lists:foldl(fun max_hero/2, undefined, State#state.game#game.heroes).
