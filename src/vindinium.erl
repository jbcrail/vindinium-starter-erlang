-module(vindinium).

-export([connect/1, connect/2, connect/3, connect/4, play/1]).

-define(SERVER, "http://vindinium.org").
-define(MODE, training).
-define(BOT, random_bot).
-define(TURNS, 300).

-record(context, {key :: string(),
                  url :: string(),
                  mode :: atom(),
                  bot_module :: atom(),
                  turns :: integer()
                 }).

%%

connect(Key) ->
    connect(Key, ?SERVER, ?MODE, ?TURNS).

connect(Key, Url) ->
    connect(Key, Url, ?MODE, ?TURNS).

connect(Key, Url, Mode) ->
    connect(Key, Url, Mode, ?TURNS).

connect(Key, Url, Mode, Turns) ->
    inets:start(),
    #context{key=Key, url=Url, mode=Mode, bot_module=?BOT, turns=Turns}.

%%

move(Context) ->
    erlang:apply(Context#context.bot_module, move, [""]).

play(Context) ->
    play(Context, initial_state(Context)).

play(Context, State) ->
    Finished = vindinium_state:finished(State),
    if
        Finished  -> State;
        true      -> play(Context, next_state(Context, State, move(Context)))
    end.

%%

post(Url, Params) ->
    Data = mochiweb_util:urlencode(Params),
    httpc:request(post, {Url, [], "application/x-www-form-urlencoded", Data}, [], []).

initial_state(Context) ->
    Url = string:join([?SERVER, "api", atom_to_list(Context#context.mode)], "/"),
    Key = Context#context.key,
    Turns = Context#context.turns,
    {ok, {{_Version, 200, _Reason}, _Headers, Body}} = post(Url, [{key, Key}, {turns, Turns}, {map, "m1"}]),
    vindinium_state:from_json(Body).

next_state(Context, State, Direction) ->
    Url = vindinium_state:play_url(State),
    Key = Context#context.key,
    {ok, {{_Version, 200, _Reason}, _Headers, Body}} = post(Url, [{key, Key}, {dir, Direction}]),
    vindinium_state:from_json(Body).
