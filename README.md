dataflow
=====

An OTP library for DataFlow computations in Erlang.

It supports building Graph structures with custom computation nodes and running them in 'push' or 'pull' mode.

Build
-----

    $ rebar3 compile
    
Example
-------
_Define 4 Nodes and connect them in a pipe fashion, start the graph in 'push' mode_

    N1 = "print1", N2 = "print2", N3 = "print3", N4 = "print4",
    
    {ok, Graph} = df_graph:start_link("graph1",[]),
    
    df_graph:add_node(Graph, N1, auto_emit, []),
    df_graph:add_node(Graph, N2, add, [2]),
    df_graph:add_node(Graph, N3, add, [3]),
    df_graph:add_node(Graph, N4, add, [4]),
    
    df_graph:add_edge(Graph, N1, 1, N2, 1, []),
    df_graph:add_edge(Graph, N2, 1, N3, 1, []),
    df_graph:add_edge(Graph, N3, 1, N4, 1, []), 
    
    df_graph:start_graph(Graph, push).
