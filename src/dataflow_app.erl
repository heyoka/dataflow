%% Date: 17.04.17 - 11:50
%% Ⓒ 2017 Alexander Minichmair
-module(dataflow_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
   application:ensure_all_started(dataflow).

start(_StartType, _StartArgs) ->
   Sup = dataflow_sup:start_link(),
   Sup.

stop(_State) ->
   ok.
