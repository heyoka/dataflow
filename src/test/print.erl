%% Date: 30.12.16 - 23:01
%% Ⓒ 2016 heyoka
-module(print).
-author("Alexander Minichmair").

-behavior(df_component).
%% API
-export([init/3, process/3, handle_info/2, inports/0, outports/0]).

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

inports() ->
   [{1, nil}].

outports() ->
   inports().

process(_Inport, Value, State=#state{node_id = NodeId}) ->
   io:format("~p process, ~p~n",[NodeId, {_Inport, Value}]),
   {emit, {1, Value*2}, State}.

handle_info(Request, State) ->
   io:format("~p request: ~p~n", [State#state.node_id, Request]),
   {ok, State}.

