%% Date: 27.12.16 - 17:45
%% Ⓒ 2016 heyoka
-module(df_graph).
-author("Alexander Minichmair").

-behaviour(gen_server).

-include("dataflow.hrl").

%% API
-export([start_link/2]).
-export([add_node/4, add_edge/6, nodes/1, edges/1, start_graph/2, stop/1]).

%% gen_server callbacks
-export([init/1,
   handle_call/3,
   handle_cast/2,
   handle_info/2,
   terminate/2,
   code_change/3]).

-record(state, {
   id                   :: non_neg_integer() | string(),
   running  = false     :: true | false,
   started  = false     :: true | false,
   graph    = nil,
   nodes    = []        :: list(tuple())
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(Id :: term(), Params :: term()) ->
   {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Id, Params) ->
   gen_server:start_link(?MODULE, [Id, Params], []).


start_graph(Graph, FlowMode) ->
   gen_server:call(Graph, {start, FlowMode}).

stop(Graph) ->
   Graph ! stop.

add_node(Graph, NodeId, Component, Metadata) ->
   gen_server:call(Graph, {add_node, NodeId, Component, Metadata}).

add_edge(Graph, SourceNode, SourcePort, TargetNode, TargetPort, Metadata) ->
   gen_server:call(Graph, {add_edge, SourceNode, SourcePort, TargetNode, TargetPort, Metadata}).

nodes(Graph) ->
   gen_server:call(Graph, {nodes}).

edges(Graph) ->
   gen_server:call(Graph, {edges}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
   {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term()} | ignore).
init([Id, _Params]) ->
   Graph = digraph:new([protected]),
   {ok, #state{graph = Graph, id = Id}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
   {reply, Reply :: term(), NewState :: #state{}} |
   {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_call({add_node, NodeId, Component, Metadata}, _From, State) ->
   Inports = df_component:inports(Component),
   OutPorts = df_component:outports(Component),

   Label = #{component => Component, component_pid => nil,
   inports => Inports, outports => OutPorts, metadata => Metadata},
   _NewVertex = digraph:add_vertex(State#state.graph, NodeId, Label),

   {reply, ok, State};
handle_call({add_edge, SourceNode, SourcePort, TargetNode, TargetPort, Metadata}, _From, State) ->
   Label = #{src_port => SourcePort, tgt_port => TargetPort, metadata => Metadata},
   _NewEdge = digraph:add_edge(State#state.graph, SourceNode, TargetNode, Label),
   {reply, ok, State};
handle_call({nodes}, _From, State) ->
   All = digraph:vertices(State#state.graph),
   {reply, All, State};
handle_call({edges}, _From, State) ->
   All = digraph:vertices(State#state.graph),
   {reply, All, State};
handle_call({start, FlowMode}, _From, State=#state{graph = G}) ->
   Nodes0 = digraph:vertices(G),

%% build : [{NodeId, Pid}]
   Nodes = lists:map(
      fun(E) ->
         {NodeId, Label} = digraph:vertex(G, E),
         #{component := Component, inports := Inports, outports := OutPorts, metadata := Metadata}
            = Label,
         {ok, Pid} = df_component:start_link(Component, NodeId, Inports, OutPorts, Metadata),
         {E, Pid}
      end, Nodes0),
   %% Inports and Subscriptions
   Subscriptions = lists:foldl(fun({NId, _N}, Acc) ->
                                   [{NId, build_subscriptions(G, NId, Nodes, FlowMode)}|Acc]
                             end, [], Nodes),

   %% start the nodes with subscriptions
   lists:foreach(
      fun({NodeId, NPid}) ->
         {Inputs, Subs} = proplists:get_value(NodeId, Subscriptions),
         df_component:start_node(NPid, Inputs, Subs, FlowMode)
      end,
      Nodes),
   %% if in pull mode initially let all components send requests to their producers
   case FlowMode of
      push -> ok;
      pull -> lists:foreach(fun({_NodeId, NPid}) -> NPid ! pull end, Nodes)
   end,

   {reply, ok, State#state{running = true, started = true, nodes = Nodes}}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
   {noreply, State}.


-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_info(stop, State=#state{running = Running, nodes = Nodes}) ->
   case Running of
      %% stop all components
      true -> lists:foreach(fun({_NodeId, NPid}) -> NPid ! stop end, Nodes);
      false -> ok
   end,
   {stop, normal, State}.


terminate(_Reason, _State) ->
   ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
   {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
build_subscriptions(Graph, Node, Nodes, FlowMode) ->
   OutEdges = digraph:out_edges(Graph, Node),
   Subscriptions = lists:foldl(
      fun(E, Acc) ->
         {_E, V1, V2, Label} = digraph:edge(Graph, E),
         #{src_port := SourcePort, tgt_port := TargetPort, metadata := _Metadata} = Label,
         S = df_subscription:new(FlowMode, proplists:get_value(V1, Nodes), SourcePort,
            proplists:get_value(V2, Nodes), TargetPort),
         case proplists:get_value(SourcePort, Acc) of
            undefined ->
               [{SourcePort,[S]}|Acc];
            PSubs when is_list((PSubs)) ->
               [{SourcePort,[S|PSubs]}|proplists:delete(SourcePort, Acc)]
         end
      end,
      [],
      OutEdges),

   InEdges = digraph:in_edges(Graph, Node),
   Inports = lists:map(
      fun(E) ->
         {_E, V1, _V2, Label} = digraph:edge(Graph, E),
         #{tgt_port := TargetPort, metadata := _Metadata} = Label,
         {TargetPort, proplists:get_value(V1, Nodes)}
      end,
      InEdges),
   {Inports, Subscriptions}.