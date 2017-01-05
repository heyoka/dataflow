%% Date: 30.12.16 - 23:01
%% Ⓒ 2016 heyoka
-module(auto_emit).
-author("Alexander Minichmair").

-behavior(df_component).
%% API
-export([init/3, process/3]).


init(_Outputs, _Opts, NodeId) ->
   io:format("~p init:node~n",[NodeId]),
%%   df_component:emit(1, random:uniform(10)),
   timer:send_interval(3000, self(), {emit, {1, random:uniform(10)}}),
   {ok, none, NodeId}.

process(_Inport, _Value, State) ->
   {ok, State}.



