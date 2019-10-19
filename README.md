Archived, because dataflow is included in faxe now !

dataflow
=====

An OTP library for DataFlow computations in Erlang.

It supports building Graph structures with custom computation nodes and running them in 'push' or 'pull' mode.

(The libary serves as a basis for a processing and analytics-framework for timeseries data.)


Build
-----

    $ rebar3 compile
    
News
----
New api for building and running a computing-graph. 

The graph will be a child of the graph_sup supervisor.

    -type graph_definition() :: #{nodes => [], edges => []}.
    
Nodes for the graph_definition are defined this way (see example below) : 
    
    {NodeName :: any(), Callback_Module :: atom(), \[Args] :: optional}
    
    
Edges between nodes :

    {NodeOut :: any(), PortOut :: non_neg_integer(), NodeIn :: any(), PortIn :: non_neg_integer(), Params :: list() optional}
    
Create a graph with graph_definition() map 

    -spec create_graph(any(), graph_definition()) -> {ok, pid()} | {error, Reason::any()}.
     
    
    {ok, Graph} = dataflow:create_graph("flow1", GraphDef).
    
Start the computation
    
    ok = dataflow:start_graph(Graph, push).
    
    
    

Example 
-------
_Define 4 Nodes and connect them in a pipe fashion, start the graph in 'push' mode_

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
    
    
Example without graph supervision
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

Custom Nodes
------------

Implement df_component behavior

    %%%===================================================================
    %%% CALLBACKS
    %%%===================================================================
    
    %% @doc
    %% INIT/3
    %%
    %% initialisation
    %% @end
    -callback init(NodeId :: term(), Inputs :: list(), Args :: term())
           -> {ok, auto_request(), cbstate()}.
    
    
    
    %% @doc
    %% PROCESS/2
    %%
    %% process value or batch incoming on specific inport
    %%
    %% return values :
    %%
    %% :: just return the state
    %% {ok, cbstate()}
    %%
    %% :: used to emit a value right after processing
    %% {emit, Port :: port(), Value :: term(), cbstate()} :: used to emit right after processing
    %%
    %% :: request a value in return of a process call
    %% {request, ReqPort :: port(), ReqPid :: pid(), cbstate()}
    %%
    %% :: emit and request a value
    %% {emit_request, OutPort :: port(), Value :: term(), ReqPort :: port(), ReqPid :: pid(), cbstate()}
    %%
    %% @end
    -callback process(Inport :: non_neg_integer(), Value :: #data_point{} | #data_batch{}, State :: cbstate())
           ->
           {ok, cbstate()} |
    
           {emit,
              { Port :: df_port(), Value :: term() }, cbstate()
           } |
           {request,
              { ReqPort :: df_port(), ReqPid :: pid() }, cbstate()
           } |
           {emit_request,
              { OutPort :: df_port(), Value :: term() }, { ReqPort :: df_port(), ReqPid :: pid() }, cbstate()
           } |
    
           {error, Reason :: term()}.
    
    
    %%%==========================================================
    %%% OPTIONAL CALLBACKS
    %%% =========================================================
    
    %% @doc
    %% OPTIONS/0
    %%
    %% optional
    %%
    %% retrieve options (with default values optionally) for a component
    %% for an optional parameter, provide Default term
    %%
    %% options with no 'Default' value, will be treated as mandatory
    %%
    
    
    -callback options() ->
    list(
       {Name :: atom(), Type :: atom(), Default :: term()} |
       {Name :: atom(), Type :: atom()}
    ). 
    
    
    %% @doc
    %% INPORTS/0
    %%
    %% optional
    %% provide a list of inports for the component :
    %%
    
    -callback inports()  -> {ok, list()}.
    
    
    
    %% @doc
    %% OUTPORTS/0
    %%
    %% optional
    %% provide a list of outports for the component:
    %%
    
    -callback outports() -> {ok, list()}. 
    
    
    
    %% @doc
    %% HANDLE_INFO/2
    %%
    %% optional
    %% handle other messages that will be sent to this process :
    %%
    
    -callback handle_info(Request :: term(), State :: cbstate()) 
        -> {ok, NewCallbackState :: cbstate()} | {error, Reason :: term()}.
    
    
    
    
    %% @doc
    %% SHUTDOWN/1
    %%
    %% optional
    %% called when component process is about to stop :
    %%
    
    -callback shutdown(State :: cbstate()) -> any().
    
    
    
    %% -optional_callbacks([inports/0, outports/0, handle_info/2, shutdown/1]). %% erlang 18+


Test
----

    % for debug console-output, add built-in event handlers
    
    dataflow:add_debug_handler().
    

    G = df:pipe2().
    
    G ! stop.
    
a view more in df module

Todo
----

* more tests
* api-docs for event-handlers
* docs for component options
* make edoc work for df_component
* single-run mode
