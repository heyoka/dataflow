%% Date: 31.12.16 - 16:59
%% Ⓒ 2016 heyoka
-module(df).
-author("Alexander Minichmair").

-include("dataflow.hrl").
%% API
-export([test/0, request/0, graph/0, pipe/0, pipe_pull/0, pipe_add/0, pipe_add_pull/0]).


pipe() ->
   N1 = "print1", N2 = "print2", N3 = "print3", N4 = "print4",
   {ok, Graph} = df_graph:start_link("graph1",[]),
   df_graph:add_node(Graph, N1, auto_emit, []),
   df_graph:add_node(Graph, N2, print, []),
   df_graph:add_node(Graph, N3, print, [node3_args_here]),
   df_graph:add_node(Graph, N4, print, []),
   df_graph:add_edge(Graph, N1, 1, N2, 1, []),
   df_graph:add_edge(Graph, N2, 1, N3, 1, []),
   df_graph:add_edge(Graph, N3, 1, N4, 1, []),
%%   Graph ! info,
   df_graph:start_graph(Graph, push).

pipe_add() ->
   N1 = "print1", N2 = "print2", N3 = "print3", N4 = "print4",
   {ok, Graph} = df_graph:start_link("graph1",[]),
   df_graph:add_node(Graph, N1, auto_emit, []),
   df_graph:add_node(Graph, N2, add, [2]),
   df_graph:add_node(Graph, N3, add, [3]),
   df_graph:add_node(Graph, N4, add, [4]),
   df_graph:add_edge(Graph, N1, 1, N2, 1, []),
   df_graph:add_edge(Graph, N2, 1, N3, 1, []),
   df_graph:add_edge(Graph, N3, 1, N4, 1, []),
%%   Graph ! info,
   df_graph:start_graph(Graph, push).

pipe_pull() ->
   N1 = "print1", N2 = "print2", N3 = "print3", N4 = "print4",
   {ok, Graph} = df_graph:start_link("graph1",[]),
   df_graph:add_node(Graph, N1, auto_emit, []),
   df_graph:add_node(Graph, N2, print, []),
   df_graph:add_node(Graph, N3, print, []),
   df_graph:add_node(Graph, N4, print, []),
   df_graph:add_edge(Graph, N1, 1, N2, 1, []),
   df_graph:add_edge(Graph, N2, 1, N3, 1, []),
   df_graph:add_edge(Graph, N3, 1, N4, 1, []),
%%   Graph ! info,
   df_graph:start_graph(Graph, pull).

pipe_add_pull() ->
   N1 = "print1", N2 = "print2", N3 = "print3", N4 = "print4",
   {ok, Graph} = df_graph:start_link("graph1",[]),
   df_graph:add_node(Graph, N1, auto_emit, []),
   df_graph:add_node(Graph, N2, add, [2]),
   df_graph:add_node(Graph, N3, add, [3]),
   df_graph:add_node(Graph, N4, add, [4]),
   df_graph:add_edge(Graph, N1, 1, N2, 1, []),
   df_graph:add_edge(Graph, N2, 1, N3, 1, []),
   df_graph:add_edge(Graph, N3, 1, N4, 1, []),
%%   Graph ! info,
   df_graph:start_graph(Graph, pull).


graph() ->
   N1 = "print1", N2 = "print2", N3 = "print3", N4 = "print4",
   {ok, Graph} = df_graph:start_link("graph1",[]),
   df_graph:add_node(Graph, N1, auto_emit, []),
   df_graph:add_node(Graph, N2, print, []),
   df_graph:add_node(Graph, N3, print, []),
   df_graph:add_node(Graph, N4, print, []),
   df_graph:add_edge(Graph, N1, 1, N2, 1, []),
   df_graph:add_edge(Graph, N1, 1, N3, 1, []),
   df_graph:add_edge(Graph, N2, 1, N3, 1, []),
   df_graph:add_edge(Graph, N3, 1, N4, 1, []),
%%   Graph ! info,
   df_graph:start_graph(Graph, push).


test() ->
   {ok, Node1} = df_component:start_link(print, "node1", [], []),
   {ok, Node2} = df_component:start_link(print, "node2", [], []),
   {ok, Node3} = df_component:start_link(print, "node3", [], []),
   F = #subscription{flow_mode = push},
   SubNode12 = F#subscription{subscriber_port = 2, subscriber_pid = Node2, publisher_port = 1, publisher_pid = Node1},
   SubNode13 = F#subscription{subscriber_port = 3, subscriber_pid = Node3, publisher_port = 1, publisher_pid = Node1},
   SubNode2 = F#subscription{subscriber_port = 3, subscriber_pid = Node3, publisher_port = 2, publisher_pid = Node2},
   df_component:start(Node1, [{1, [SubNode12, SubNode13]}, {2, [SubNode12, SubNode13]}]),
   df_component:start(Node2, [{2, [SubNode2]}] ),

   io:format("### Node1 Port 1 push to Node 2 Port 2 and Node 3 Port 3~n"),
   df_subscription:output( [{1, [SubNode12, SubNode13]}, {2, [SubNode12, SubNode13]}], 12, 1)
%%   df_subscription:output( [{1, SubNode12}], <<"val1">>, 1),
%%   timer:sleep(50),
%%   io:format("### Node2 Port 2 push to Node 3 Port 3 ~n"),
%%   df_subscription:output( [{2, SubNode2}], <<"val2">>, 2).
.

request() ->
   {ok, Node1} = df_component:start_link(print, "node1", [], []),
   {ok, Node2} = df_component:start_link(print, "node2", [], []),
   {ok, Node3} = df_component:start_link(print, "node3", [], []),
   F = #subscription{flow_mode = pull, out_buffer = queue:new()},
   SubNode12 = F#subscription{subscriber_port = 2, subscriber_pid = Node2, publisher_port = 1, publisher_pid = Node1},
   SubNode13 = F#subscription{subscriber_port = 3, subscriber_pid = Node3, publisher_port = 1, publisher_pid = Node1},
   SubNode2 = F#subscription{subscriber_port = 3, subscriber_pid = Node3, publisher_port = 2, publisher_pid = Node2},
   df_component:start(Node1, [{1, [SubNode12, SubNode13]}, {2, [SubNode12, SubNode13]}]),
   df_component:start(Node2, [{2, [SubNode2]}] ),

   io:format("### Node1 Port 1 push to Node 2 Port 2 and Node 3 Port 3~n"),
   New = df_subscription:output( [{1, [SubNode12, SubNode13]}, {2, [SubNode12, SubNode13]}], 12, 1),
   df_component:start(Node1, New),
   New2 = df_subscription:output( [{2, SubNode2}], 4, 2),
   df_component:start(Node2, New2),
   Node1 ! {request, Node2, all},
   timer:sleep(1000),
   Node2 ! {request, Node3, all}
%%   df_subscription:output( [{1, SubNode12}], <<"val1">>, 1),
%%   timer:sleep(50),
%%   io:format("### Node2 Port 2 push to Node 3 Port 3 ~n"),
%%   df_subscription:output( [{2, SubNode2}], <<"val2">>, 2).
.