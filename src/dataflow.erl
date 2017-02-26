-module(dataflow).

%% API exports
-export([request_items/2, emit/1, build_options/2]).

%%====================================================================
%% CALLBACK API functions
%%====================================================================

request_items(Port, PublisherPids) when is_list(PublisherPids) ->
   [Pid ! {request, self(), Port} || Pid <- PublisherPids].

emit(Value) ->
   emit(1, Value).
emit(Port, Value) ->
   erlang:send_after(0, self(), {emit, {Port, Value}}).


-spec build_options(atom(), list({atom(), term()})) -> map().
build_options(Component, L) ->
   Opts = case erlang:function_exported(Component, options, 0) of
             true -> io:format("Module ~p has function options/0 exported~n",[Component]),Component:options();
             false -> io:format("Module ~p has NO function options/0 exported~n",[Component]),[]
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
val(Val, string) when is_binary(Val) -> Val;
val(Val, list) when is_list(Val) -> Val;
val(Val, atom) when is_atom(Val) -> Val;
val(true, bool) -> true;
val(false, bool) -> false;
val(V, Type) -> erlang:error({wrong_option_type, {V, Type}}).
