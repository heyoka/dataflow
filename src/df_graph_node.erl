%% Date: 01.01.17 - 18:43
%% Ⓒ 2017 LineMetrics GmbH
-module(df_graph_node).
-author("Alexander Minichmair").


%% @doc
%% provide a list of inports for the component
%% @end
-callback inports()  -> {ok, list()}.

%% @doc
%% provide a list of outports for the component
%% @end
-callback outports() -> {ok, list()}.