-module(dataflow).

%% API exports
-export([new_graph/0, create_graph/2, start_graph/1, start_graph/2, add_node/2, add_edge/2]).

-export([request_items/2, emit/1, build_options/2]).


-type graph_definition() :: #{nodes => [], edges => []}.

%%====================================================================
%% CALLBACK API functions
%%====================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc get a new graph definition map
-spec new_graph() -> graph_definition().
new_graph() ->
   #{nodes => [], edges => []}.

%% @doc graph_definition() : add graph node
-spec add_node({any(), atom()} | {any(), atom(), list()}, graph_definition()) -> graph_definition().
add_node({NodeName, Component}, Defs) when is_atom(Component), is_map(Defs) ->
   add_node({NodeName, Component, []}, Defs);
add_node({NodeName, Component, Params}, Defs=#{nodes := Nodes}) when is_atom(Component), is_map(Defs), is_list(Params) ->
   Defs#{nodes := [{NodeName, Component, Params} | Nodes]}.

%% @doc add a new edge to #{nodes := Nodes, edges := Edges}
-spec add_edge(tuple(), graph_definition()) -> graph_definition().
add_edge({NodeOut, PortOut, NodeIn, PortIn}, Defs) ->
   add_edge({NodeOut, PortOut, NodeIn, PortIn, []}, Defs);
add_edge({NodeOut, PortOut, NodeIn, PortIn, Params}, Defs = #{edges := Edges}) ->
   Defs#{edges := [{NodeOut, PortOut, NodeIn, PortIn, Params} | Edges]}.

%% @doc start a new df_graph process
-spec create_graph(any(), graph_definition()) -> {ok, pid()} | {error, Reason::any()}.
create_graph(Id, Definitions) when is_map(Definitions) ->
   Res = graph_sup:new(Id, Definitions),
   io:format("new graph: ~p~n",[Res]),
   Res.

%% @doc start the graph computation
start_graph(Graph) ->
   start_graph(Graph, push).
start_graph(Graph, Mode) ->
   df_graph:start_graph(Graph, Mode).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% component functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
request_items(Port, PublisherPids) when is_list(PublisherPids) ->
   [Pid ! {request, self(), Port} || Pid <- PublisherPids].

emit(Value) ->
   emit(1, Value).
emit(Port, Value) ->
   erlang:send_after(0, self(), {emit, {Port, Value}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec build_options(atom(), list({atom(), term()})) -> map().
build_options(Component, L) ->
   Opts = case erlang:function_exported(Component, options, 0) of
             true -> io:format("Module ~p has function options/0 exported~n",[Component]),Component:options();
             false -> %io:format("Module ~p has NO function options/0 exported~n",[Component]),
                        []
          end,
   do_build_options(Opts, L).
do_build_options([], _) -> #{};
do_build_options(Opts, L) when is_list(L), is_list(Opts) ->
   lists:foldl(
      fun
         ({OptName, is_set}, Acc) ->
            case proplists:get_value(OptName, L) of
               undefined -> Acc#{OptName => false};
               true      -> Acc#{OptName => true}
            end;
         ({OptName, OptType, Default}, Acc) ->
            case proplists:get_value(OptName, L) of
               undefined -> Acc#{OptName => Default};
               V        -> Acc#{OptName => val(V, OptType)}
            end;
         ({OptName, OptType}, Acc) ->
            case proplists:get_value(OptName, L) of
               undefined -> erlang:error({option_missing, OptName});
               V        -> Acc#{OptName => val(V, OptType)}
            end
      end,
      #{},
      Opts).

%%====================================================================
%% Internal functions
%%====================================================================
val(Val, number) when is_integer(Val) orelse is_float(Val) -> Val;
val(Val, integer) when is_integer(Val) -> Val;
val(Val, float) when is_float(Val) -> Val;
val(Val, binary) when is_binary(Val) -> Val;
val(Val, string) when is_binary(Val) -> Val;
val(Val, list) when is_list(Val) -> Val;
val(Val, atom) when is_atom(Val) -> Val;
val(true, bool) -> true;
val(false, bool) -> false;
val(Val, lambda) when is_function(Val) -> Val;
val(V, Type) -> erlang:error({wrong_option_type, {V, Type}}).
