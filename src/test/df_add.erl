%% Date: 02.01.17 - 15:42
%% Ⓒ 2017 heyoka
-module(df_add).
-author("Alexander Minichmair").

-behavior(df_component).

%% API
-export([init/3, process/3, options/0]).

-record(state, {
   addend,
   node_id
}).

options() ->
   [{addend, number}].

init(NodeId, _Inputs, #{addend := Addend}) ->
   {ok, all, #state{addend = Addend, node_id = NodeId}}.

process(_Inport, Value, State = #state{addend = Add}) ->
   NewValue = Value + Add,
   {emit, NewValue, State}.