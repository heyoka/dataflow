%% Date: 02.01.17 - 15:42
%% Ⓒ 2017 LineMetrics GmbH
-module(add).
-author("Alexander Minichmair").

-behavior(df_component).

%% API
-export([init/3, inports/0, outports/0, process/3, handle_info/2]).

-record(state, {
   add,
   node_id
}).

init(NodeId, _Inputs, [Args]) ->
   {ok, all, #state{add = Args, node_id = NodeId}}.

inports() ->
   [{1, nil}].

outports() ->
   inports().

process(_Inport, Value, State = #state{add = Add}) ->
   NewValue = Value + Add,
   {emit, {1, NewValue}, State}.

handle_info(_Request, State) ->
   {ok, State}.