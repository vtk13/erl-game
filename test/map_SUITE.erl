-module(map_SUITE).
-author("vtk").

-compile(export_all).

-include("../include/game.hrl").

all() ->
    [map_intersect, map_move].

map_intersect(_) ->
    {ok, Map} = game_map:start_link(),

    Unit1 = #unit{id = 1, x = 50, y = 50, hw = 10, hh = 10},
    ok = game_map:add(Map, Unit1),

    Unit2 = #unit{id = 2, x = 30, y = 50, hw = 10, hh = 10},
    ok = game_map:add(Map, Unit2),

    [Unit2] = game_map:intersect(Map, {25, 50, 10, 10}),
    [Unit1] = game_map:intersect(Map, {55, 50, 10, 10}),
    %% TODO why 2 then 1? ordered_set?
    [Unit2, Unit1] = game_map:intersect(Map, {40, 50, 10, 10}).

map_move(_) ->
    {ok, Map} = game_map:start_link(),

    Unit1 = #unit{id = 1, x = 50, y = 50, hw = 10, hh = 10},
    ok = game_map:add(Map, Unit1),

    [Unit1] = game_map:intersect(Map, {60, 50, 10, 10}),

    ok = game_map:move(Map, Unit1, {0, 0}),
    [] = game_map:intersect(Map, {60, 50, 10, 10}),
    [Unit1] = game_map:intersect(Map, {-5, -5, 10, 10}).
