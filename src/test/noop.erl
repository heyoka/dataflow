%% Date: 30.12.16 - 23:01
%% Ⓒ 2016 heyoka
-module(noop).
-author("Alexander Minichmair").

-behavior(df_component).
%% API
-export([init/3, process/3]).


init(NodeId, _Inputs, _Args) ->
   io:format("~p init:node~n",[NodeId]),
   {ok, all, NodeId}.

process(_Inport, Value, State) ->
   io:format("~p process, ~p~n",[State, {_Inport, Value}]),

   {emit, {1, Value}, State}.
