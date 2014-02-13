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
          tiles
         }).

-record(position, {
          x,
          y
         }).
