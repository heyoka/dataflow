%% Date: 30.12.16 - 23:01
%% Ⓒ 2016 heyoka
-module(noop).
-author("Alexander Minichmair").

-behavior(df_component).
%% API
-export([init/3, process/3, handle_info/2, inports/0, outports/0]).


init(NodeId, _Inputs, _Args) ->
   io:format("~p init:node~n",[NodeId]),
   {ok, all, NodeId}.

inports() ->
   [{1, nil}].

outports() ->
   inports().

process(_Inport, Value, State) ->
   io:format("~p process, ~p~n",[State, {_Inport, Value}]),

   {emit, {1, Value*2}, State}.

handle_info(Request, State) ->
   io:format("~p request: ~p~n", [State, Request]),
   {ok, State}.

