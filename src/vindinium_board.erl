-module(vindinium_board).
-include("vindinium.hrl").

-export([to_directions/2, valid_moves/1, nearest_opponent/1, nearest_tavern/1, nearest_unowned_mine/1]).

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

direction(Path) when length(Path) >= 2 ->
    {I1, _} = lists:nth(1, Path),
    {I2, _} = lists:nth(2, Path),
    if
        I2-I1 =:= -1 ->
            "West";
        I2-I1 =:= 1 ->
            "East";
        I2-I1 < 0 ->
            "North";
        I2-I1 > 0 ->
            "South";
        true ->
            "Stay"
    end;
direction(_) ->
    "Stay".

find_shortest_path(Graph, Origin, Vertices) ->
    Fun = fun(V) ->
               Path = digraph:get_short_path(Graph, Origin, V),
               case Path of
                  false -> false;
                  _ -> {true, {length(Path), Path}}
               end
          end,
    Paths = lists:filtermap(Fun, Vertices),
    case Paths of
        [] -> [];
        _ -> {_, Path} = lists:min(lists:filtermap(Fun, Vertices)),
                Path
    end.

is_opponent(Type) ->
    Type =:= hero2 orelse Type =:= hero3 orelse Type =:= hero4.

is_unowned_mine(Type) ->
    Type =:= neutral_mine orelse Type =:= mine2 orelse Type =:= mine3 orelse Type =:= mine4.

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
    Opponents = lists:filter(fun({_, Type}) -> is_opponent(Type) end, digraph:vertices(G)),
    Taverns = lists:filter(fun({_, Type}) -> Type =:= tavern end, digraph:vertices(G)),
    UnownedMines = lists:filter(fun({_, Type}) -> is_unowned_mine(Type) end, digraph:vertices(G)),
    #directions{
      valid=lists:map(fun(V2) -> direction([HeroVertex, V2]) end, digraph:out_neighbours(G, HeroVertex)) ++ ["Stay"],
      hero2=direction(digraph:get_short_path(G, HeroVertex, Hero2)),
      hero3=direction(digraph:get_short_path(G, HeroVertex, Hero3)),
      hero4=direction(digraph:get_short_path(G, HeroVertex, Hero4)),
      nearest_opponent=direction(find_shortest_path(G, HeroVertex, Opponents)),
      nearest_tavern=direction(find_shortest_path(G, HeroVertex, Taverns)),
      nearest_unowned_mine=direction(find_shortest_path(G, HeroVertex, UnownedMines))
      }.

valid_moves(State) ->
    State#state.game#game.board#board.directions#directions.valid.

nearest_opponent(State) ->
    State#state.game#game.board#board.directions#directions.nearest_opponent.

nearest_tavern(State) ->
    State#state.game#game.board#board.directions#directions.nearest_tavern.

nearest_unowned_mine(State) ->
    State#state.game#game.board#board.directions#directions.nearest_unowned_mine.
