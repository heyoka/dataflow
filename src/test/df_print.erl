%% Date: 30.12.16 - 23:01
%% Ⓒ 2016 heyoka
-module(df_print).
-author("Alexander Minichmair").

-behavior(df_component).

-include("dataflow.hrl").
%% API
-export([init/3, process/3, shutdown/1]).

-record(state, {
   inputs,
   publishers,
   node_id,
   arguments
}).

init(NodeId, Inputs, Args) ->
   ?LOG("~p init",[NodeId]),
   {_Ports, Publishers} = lists:unzip(Inputs),
   {ok, emit,
      #state{inputs = Inputs, node_id = NodeId, arguments = Args, publishers = Publishers}}.

process(_Inport, Value, State=#state{node_id = NodeId}) ->
   ?LOG("~p process, ~p",[NodeId, {_Inport, Value}]),
   {emit, {1, Value*2}, State}.

shutdown(_State) ->
   ?LOG("shutdown in ~p called",[?MODULE]).


