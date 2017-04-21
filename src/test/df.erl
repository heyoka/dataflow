%% Date: 31.12.16 - 16:59
%% Ⓒ 2016 heyoka
-module(df).
-author("Alexander Minichmair").

-include("dataflow.hrl").
%% API
-export([tree/0, pipe/0, pipe_pull/0, pipe_add/0, pipe_add_pull/0, pipe_stop/0, pipe2/0, pipe2/1]).


pipe_stop() ->
   G = pipe(),
   erlang:send_after(7000, G, stop).
%%   G ! stop.

pipe() ->
   N1 = "print1", N2 = "print2", N3 = "print3", N4 = "print4",
   {ok, Graph} = df_graph:start_link("graph1",[]),
   df_graph:add_node(Graph, N1, df_auto_emit, []),
   df_graph:add_node(Graph, N2, df_print, []),
   df_graph:add_node(Graph, N3, df_print, [node3_args_here]),
   df_graph:add_node(Graph, N4, df_print, []),
   df_graph:add_edge(Graph, N1, 1, N2, 1),
   df_graph:add_edge(Graph, N2, 1, N3, 1, []),
   df_graph:add_edge(Graph, N3, 1, N4, 1),
%%   Graph ! info,
   df_graph:start_graph(Graph, push),
   Graph.

%% @doc use new dataflow api
pipe2() ->
   pipe2("graph_p").
pipe2(G) ->
   N1 = "p1", N2 = "p2", N3 = "p3", N4 = "p4",
   Nodes = [
      {N1, df_auto_emit},
      {N2, df_print},
      {N3, df_print, [node3_args_here]},
      {N4, df_print}
   ],
   Edges = [
      {N1, 1, N2, 1},
      {N2, 1, N3, 1},
      {N3, 1, N4, 1}
   ],
   GraphDef = #{nodes => Nodes, edges => Edges},
   {ok, Graph} = dataflow:create_graph(G, GraphDef),
   dataflow:start_graph(Graph, push),
   Graph.

pipe_add() ->
   N1 = "print1", N2 = "print2", N3 = "print3", N4 = "print4",
   {ok, Graph} = df_graph:start_link("graph1",[]),
   df_graph:add_node(Graph, N1, df_auto_emit, []),
   df_graph:add_node(Graph, N2, df_add, [{addend, 2}]),
   df_graph:add_node(Graph, N3, df_add, [{addend, 3}]),
   df_graph:add_node(Graph, N4, df_add, [{addend, 4}]),
   df_graph:add_edge(Graph, N1, 1, N2, 1, []),
   df_graph:add_edge(Graph, N2, 1, N3, 1, []),
   df_graph:add_edge(Graph, N3, 1, N4, 1, []),
%%   Graph ! info,
   df_graph:start_graph(Graph, push).

pipe_pull() ->
   N1 = "print1", N2 = "print2", N3 = "print3", N4 = "print4",
   {ok, Graph} = df_graph:start_link("graph1",[]),
   df_graph:add_node(Graph, N1, df_auto_emit, []),
   df_graph:add_node(Graph, N2, df_print, []),
   df_graph:add_node(Graph, N3, df_print, []),
   df_graph:add_node(Graph, N4, df_print, []),
   df_graph:add_edge(Graph, N1, 1, N2, 1, []),
   df_graph:add_edge(Graph, N2, 1, N3, 1, []),
   df_graph:add_edge(Graph, N3, 1, N4, 1, []),
%%   Graph ! info,
   df_graph:start_graph(Graph, pull).

pipe_add_pull() ->
   N1 = "print1", N2 = "print2", N3 = "print3", N4 = "print4",
   {ok, Graph} = df_graph:start_link("graph1",[]),
   df_graph:add_node(Graph, N1, df_auto_emit, []),
   df_graph:add_node(Graph, N2, df_add, [2]),
   df_graph:add_node(Graph, N3, df_add, [3]),
   df_graph:add_node(Graph, N4, df_add, [4]),
   df_graph:add_edge(Graph, N1, 1, N2, 1, []),
   df_graph:add_edge(Graph, N2, 1, N3, 1, []),
   df_graph:add_edge(Graph, N3, 1, N4, 1, []),
%%   Graph ! info,
   df_graph:start_graph(Graph, pull).


tree() ->
   N1 = "print1", N2 = "print2", N3 = "print3", N4 = "print4",
   {ok, Graph} = df_graph:start_link("graph1",[]),
   df_graph:add_node(Graph, N1, df_auto_emit, []),
   df_graph:add_node(Graph, N2, df_print, []),
   df_graph:add_node(Graph, N3, df_print, []),
   df_graph:add_node(Graph, N4, df_print, []),
   df_graph:add_edge(Graph, N1, 1, N2, 1, []),
   df_graph:add_edge(Graph, N1, 1, N3, 1, []),
   df_graph:add_edge(Graph, N2, 1, N3, 1, []),
   df_graph:add_edge(Graph, N3, 1, N4, 1, []),
%%   Graph ! info,
   df_graph:start_graph(Graph, push),
   Graph.
