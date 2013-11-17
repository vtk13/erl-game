-module(game_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    start(0, 0).

start(_StartType, _StartArgs) ->
    game_sup:start_link().

stop(_State) ->
    ok.
