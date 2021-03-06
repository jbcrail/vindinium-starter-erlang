-module(vindinium).
-include("vindinium.hrl").

-export([connect/1, connect/2, connect/3, connect/4, connect/5, play/1]).

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
    connect(Key, Url, ?MODE, ?BOT, ?TURNS).

%% @spec connect(string(), string(), atom()) ->
%%       {ok, context()} |
%%       {error, Err :: term()}
%% @doc Connect to Vindinium server.  Return a context value or error.
connect(Key, Url, Mode) ->
    connect(Key, Url, Mode, ?BOT, ?TURNS).

%% @spec connect(string(), string(), atom(), atom()) ->
%%       {ok, context()} |
%%       {error, Err :: term()}
%% @doc Connect to Vindinium server.  Return a context value or error.
connect(Key, Url, Mode, Bot) ->
    connect(Key, Url, Mode, Bot, ?TURNS).

%% @spec connect(string(), string(), atom(), atom(), integer()) ->
%%       {ok, context()} |
%%       {error, Err :: term()}
%% @doc Connect to Vindinium server.  Return a context value or error.
connect(Key, Url, Mode, Bot, Turns) ->
    case inets:start() of
        ok ->
            {ok, #context{key=Key, url=Url, mode=Mode, bot_module=Bot, turns=Turns}};
        {error, Reason} ->
            {error, Reason}
    end.

move(Context, State) ->
    erlang:apply(Context#context.bot_module, move, [State]).

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
    case vindinium_state:finished(State) of
        true ->
            {ok, State};
        false ->
            play(Context, next_state(Context, State, move(Context, State)))
    end;
play(_Context, Error) ->
    Error.
