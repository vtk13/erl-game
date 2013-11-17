%% Copyright
-module(game_map).
-author("vtk").

-behaviour(gen_server).

-include("include/game.hrl").

%% API
-export([start_link/0, add/3, get/2, remove/2, intersect/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Pid, Item, XY = {_X, _Y}) ->
    gen_server:call(Pid, {add, Item, XY}).

get(Pid, XY = {_X, _Y}) ->
    gen_server:call(Pid, {get, XY});
get(Pid, ItemId) ->
    gen_server:call(Pid, {get, ItemId}).

remove(Pid, ItemId) ->
    gen_server:call(Pid, {remove, ItemId}).

intersect(Pid, Rect = {_X, _Y, _HW, _HH}) ->
    gen_server:call(Pid, {intersect, Rect}).

%% gen_server callbacks
-record(state, {id_table, xy_table}).

init(_Args) ->
    IdTable = ets:new(map, [set, {keypos, 2}]),
    XYTable = ets:new(map, [set]),
    {ok, #state{id_table = IdTable, xy_table = XYTable}}.

handle_call({add, Item, XY = {_X, _Y}}, _From, State) ->
    ets:insert(State#state.id_table, Item),
    ets:insert(State#state.xy_table, {XY, Item}),
    {reply, ok, State};
handle_call({get, XY = {_X, _Y}}, _From, State) ->
    [{XY, Item}] = ets:lookup(State#state.xy_table, XY),
    {reply, Item, State};
handle_call({get, ItemId}, _From, State) ->
    [Item] = ets:lookup(State#state.id_table, ItemId),
    {reply, Item, State};
handle_call({intersect, Rect = {X, Y, HW, HH}}, _From, State) ->
    XYTable = State#state.xy_table,
    Units = ets:foldl(fun({XY = {UX, UY}, Unit}, Acc) ->
        Acc
    end, [], XYTable),
    {reply, Units, State};
handle_call(_Request, _From, State) ->
    {reply, invalid_request, State}.

handle_cast(Request, State) ->
    erlang:display(Request),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
