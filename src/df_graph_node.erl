%% Date: 01.01.17 - 18:43
%% Ⓒ 2017 heyoka
-module(df_graph_node).
-author("Alexander Minichmair").


-callback inports(Module :: atom())  -> {ok, list()}.

-callback outports(Module :: atom()) -> {ok, list()}.