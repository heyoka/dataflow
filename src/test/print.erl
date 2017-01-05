%% Date: 30.12.16 - 23:01
%% Ⓒ 2016 heyoka
-module(print).
-author("Alexander Minichmair").

-behavior(df_component).
%% API
-export([init/3, process/3]).

-record(state, {
   inputs,
   publishers,
   node_id,
   arguments
}).

init(NodeId, Inputs, Args) ->
   io:format("~p init~n",[NodeId]),
   {_Ports, Publishers} = lists:unzip(Inputs),
   {ok, emit,
      #state{inputs = Inputs, node_id = NodeId, arguments = Args, publishers = Publishers}}.

process(_Inport, Value, State=#state{node_id = NodeId}) ->
   io:format("~p process, ~p~n",[NodeId, {_Inport, Value}]),
   {emit, {1, Value*2}, State}.


