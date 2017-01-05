%% Date: 02.01.17 - 15:42
%% Ⓒ 2017 heyoka
-module(add).
-author("Alexander Minichmair").

-behavior(df_component).

%% API
-export([init/3, process/3]).

-record(state, {
   add,
   node_id
}).

init(NodeId, _Inputs, [Args]) ->
   {ok, all, #state{add = Args, node_id = NodeId}}.

process(_Inport, Value, State = #state{add = Add}) ->
   NewValue = Value + Add,
   {emit, {1, NewValue}, State}.