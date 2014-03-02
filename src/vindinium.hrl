-define(SERVER, "http://vindinium.org").
-define(MODE, training).
-define(BOT, random_bot).
-define(TURNS, 300).

-record(context, {
          key :: string(),
          url :: string(),
          mode :: atom(),
          bot_module :: atom(),
          turns :: integer()
         }).

-type context() :: #context{}.

-record(state, {
          game,
          hero,
          token,
          view_url,
          play_url
         }).

-record(game, {
          id,
          turn,
          max_turns,
          heroes,
          board,
          finished
         }).

-record(hero, {
          id,
          name,
          user_id,
          elo,
          pos,
          life,
          gold,
          mine_count,
          spawn_pos,
          crashed
         }).

-record(board, {
          size,
          tiles,
          directions
         }).

-record(position, {
          x,
          y
         }).

-record(directions, {
          valid :: list(),
          hero2 :: string(),
          hero3 :: string(),
          hero4 :: string(),
          nearest_hero :: string(),
          nearest_tavern :: string(),
          nearest_neutral_mine :: string() | undefined
         }).
