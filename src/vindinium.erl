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

-type context() :: #context{}.

%% @spec connect(string()) ->
%%       {ok, context()} |
%%       {error, Err :: term()}
%% @doc Connect to Vindinium server.  Return a context value or error.
connect(Key) ->
    connect(Key, ?SERVER, ?MODE, ?TURNS).

%% @spec connect(string(), string()) ->
%%       {ok, context()} |
%%       {error, Err :: term()}
%% @doc Connect to Vindinium server.  Return a context value or error.
connect(Key, Url) ->
    connect(Key, Url, ?MODE, ?TURNS).

%% @spec connect(string(), string(), atom()) ->
%%       {ok, context()} |
%%       {error, Err :: term()}
%% @doc Connect to Vindinium server.  Return a context value or error.
connect(Key, Url, Mode) ->
    connect(Key, Url, Mode, ?TURNS).

%% @spec connect(string(), string(), atom(), integer()) ->
%%       {ok, context()} |
%%       {error, Err :: term()}
%% @doc Connect to Vindinium server.  Return a context value or error.
connect(Key, Url, Mode, Turns) ->
    case inets:start() of
        ok ->
            {ok, #context{key=Key, url=Url, mode=Mode, bot_module=?BOT, turns=Turns}};
        {error, Reason} ->
            {error, Reason}
    end.

move(Context) ->
    erlang:apply(Context#context.bot_module, move, [""]).

post(Url, Params) ->
    Data = mochiweb_util:urlencode(Params),
    Response = httpc:request(post, {Url, [], "application/x-www-form-urlencoded", Data}, [], []),
    case Response of
        {ok, {{_Version, 200, _Reason}, _Headers, Body}} ->
            {ok, vindinium_state:from_json(Body)};
        {ok, {{_Version, Status, Reason}, _Headers, _Body}} ->
            {error, Status, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

initial_state(Context) ->
    Url = string:join([?SERVER, "api", atom_to_list(Context#context.mode)], "/"),
    Key = Context#context.key,
    Turns = Context#context.turns,
    post(Url, [{key, Key}, {turns, Turns}, {map, "m1"}]).

next_state(Context, State, Direction) ->
    Url = vindinium_state:play_url(State),
    Key = Context#context.key,
    post(Url, [{key, Key}, {dir, Direction}]).

%% @spec play(context()) ->
%%       {ok, state()} |
%%       {error, Reason :: string()} |
%%       {error, Status :: integer(), Reason :: string()}
%% @doc Advance game until completion or an error is encountered.
%%      Return a state value or error.
play(Context) ->
    play(Context, initial_state(Context)).

play(Context, {ok, State}) ->
    Finished = vindinium_state:finished(State),
    if
        Finished  -> {ok, State};
        true      -> play(Context, next_state(Context, State, move(Context)))
    end;
play(_Context, Error) ->
    Error.
