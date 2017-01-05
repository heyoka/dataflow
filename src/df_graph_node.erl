%% Date: 01.01.17 - 18:43
%% Ⓒ 2017 heyoka
-module(df_graph_node).
-author("Alexander Minichmair").


%% @doc
%% provide a list of inports for the component
%% @end
-callback inports(Module :: atom())  -> {ok, list()}.

%% @doc
%% provide a list of outports for the component
%% @end
-callback outports(Module :: atom()) -> {ok, list()}.