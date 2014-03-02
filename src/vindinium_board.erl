-module(vindinium_board).
-include("vindinium.hrl").

-export([to_directions/2, valid_moves/1, nearest_opponent/1, nearest_tavern/1, nearest_neutral_mine/1]).

% Partition given list into n-sized chunks
chunks(List, N) ->
    lists:map(fun(I) -> lists:sublist(List, I, N) end, lists:seq(1, length(List), N)).

to_type("  ") -> empty;
to_type("##") -> woods;
to_type("[]") -> tavern;
to_type("@1") -> hero1;
to_type("@2") -> hero2;
to_type("@3") -> hero3;
to_type("@4") -> hero4;
to_type("$-") -> neutral_mine;
to_type("$1") -> mine1;
to_type("$2") -> mine2;
to_type("$3") -> mine3;
to_type("$4") -> mine4.

valid_moves({I, _}, Vertices, Size) ->
    Invalid = {false, {0, nil}},
    Lmove = if
                I rem Size =:= 0 ->
                    Invalid;
                true ->
                    {true, array:get(I-1, Vertices)}
            end,
    Rmove = if
                I rem Size =:= Size-1 ->
                    Invalid;
                true ->
                    {true, array:get(I+1, Vertices)}
            end,
    Dmove = if
                I div Size =:= 0 ->
                    Invalid;
                true ->
                    {true, array:get(I-Size, Vertices)}
            end,
    Umove = if
                I div Size =:= Size-1 ->
                    Invalid;
                true ->
                    {true, array:get(I+Size, Vertices)}
            end,
    lists:map(fun({_, V}) -> V end, lists:filter(fun({Bool, {_, Type}}) -> Bool andalso Type =/= woods end, [Lmove, Rmove, Dmove, Umove])).

add_edges(G, V1, Vertices, Size) ->
    {_, Type} = V1,
    if
        Type =:= hero1 orelse Type =:= empty ->
            lists:foreach(fun(V2) -> digraph:add_edge(G, V1, V2) end, valid_moves(V1, Vertices, Size));
        true ->
            ok
    end.

direction({_, [{I1, _}, {I2, _} | _]}) when I2-I1 =:= -1 ->
    "Left";
direction({_, [{I1, _}, {I2, _} | _]}) when I2-I1 =:= 1 ->
    "Right";
direction({_, [{I1, _}, {I2, _} | _]}) when I2-I1 < 0 ->
    "Down";
direction({_, [{I1, _}, {I2, _} | _]}) when I2-I1 > 0 ->
    "Up";
direction(_) ->
    "Stay".

to_directions(Size, Tiles) ->
    Chunks = lists:map(fun(Chunk) -> to_type(Chunk) end, chunks(binary_to_list(Tiles), 2)),
    Vertices = array:map(fun(I, Val) -> {I, Val} end, array:from_list(Chunks)),
    G = digraph:new(),
    lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, array:to_list(Vertices)),
    lists:foreach(fun(V) -> add_edges(G, V, Vertices, Size) end, array:to_list(Vertices)),
    HeroVertex = lists:nth(1, lists:filter(fun({_, Type}) -> Type =:= hero1 end, digraph:vertices(G))),
    Hero2 = lists:nth(1, lists:filter(fun({_, Type}) -> Type =:= hero2 end, digraph:vertices(G))),
    Hero3 = lists:nth(1, lists:filter(fun({_, Type}) -> Type =:= hero3 end, digraph:vertices(G))),
    Hero4 = lists:nth(1, lists:filter(fun({_, Type}) -> Type =:= hero4 end, digraph:vertices(G))),
    Heroes = lists:filter(fun({_, Type}) -> Type =:= hero2 orelse Type =:= hero3 orelse Type =:= hero4 end, digraph:vertices(G)),
    Taverns = lists:filter(fun({_, Type}) -> Type =:= tavern end, digraph:vertices(G)),
    NeutralMines = lists:filter(fun({_, Type}) -> Type =:= neutral_mine end, digraph:vertices(G)),
    #directions{
      valid=lists:map(fun(V2) -> direction({1, [HeroVertex, V2]}) end, digraph:out_neighbours(G, HeroVertex)) ++ ["Stay"],
      hero2=direction({0, digraph:get_short_path(G, HeroVertex, Hero2)}),
      hero3=direction({0, digraph:get_short_path(G, HeroVertex, Hero3)}),
      hero4=direction({0, digraph:get_short_path(G, HeroVertex, Hero4)}),
      nearest_hero=direction(lists:min(lists:map(fun(V) -> Path = digraph:get_short_path(G, HeroVertex, V), {length(Path), Path} end, Heroes))),
      nearest_tavern=direction(lists:min(lists:map(fun(V) -> Path = digraph:get_short_path(G, HeroVertex, V), {length(Path), Path} end, Taverns))),
      nearest_neutral_mine=direction(lists:min(lists:map(fun(V) -> Path = digraph:get_short_path(G, HeroVertex, V), {length(Path), Path} end, NeutralMines)))
      }.

valid_moves(State) ->
    State#state.game#game.board#board.directions#directions.valid.

nearest_opponent(State) ->
    State#state.game#game.board#board.directions#directions.nearest_hero.

nearest_tavern(State) ->
    State#state.game#game.board#board.directions#directions.nearest_tavern.

nearest_neutral_mine(State) ->
    State#state.game#game.board#board.directions#directions.nearest_neutral_mine.
