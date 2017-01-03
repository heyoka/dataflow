%% Date: 30.12.16 - 23:01
%% Ⓒ 2016 heyoka
-module(auto_emit).
-author("Alexander Minichmair").

-behavior(df_component).
%% API
-export([init/3, process/3, handle_info/2, inports/0, outports/0]).


init(_Outputs, _Opts, NodeId) ->
   io:format("~p init:node~n",[NodeId]),
%%   df_component:emit(1, random:uniform(10)),
   timer:send_interval(3000, self(), {emit, {1, random:uniform(10)}}),
   {ok, none, NodeId}.

inports() ->
   [{1, nil}].

outports() ->
   inports().

process(_Inport, Value, State) ->
   io:format("~p process, ~p~n",[State, {_Inport, Value}]),

   {ok, State}.

handle_info(Req, State) ->
   io:format("~p request: ~p~n", [State, Req]),
   {ok, State}.



