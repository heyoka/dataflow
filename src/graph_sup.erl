%% Date: 16.04.17 - 22:14
%% Ⓒ 2017 Alexander Minichmair
-module(graph_sup).
-author("Alexander Minichmair").

-behaviour(supervisor).

%% API
-export([start_link/0, new/2]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
   {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init([]) ->
   {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
      MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
      [ChildSpec :: supervisor:child_spec()]
   }} |
   ignore |
   {error, Reason :: term()}).
init([]) ->
   RestartStrategy = one_for_one,
   MaxRestarts = 1000,
   MaxSecondsBetweenRestarts = 3600,

   SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

   {ok, {SupFlags, []}}.


%% @doc graph_sup starts a new graph_sup, via the dataflow_sup supervisor
new(Id, Defs) ->
   supervisor:start_child(?MODULE, child(Id, Defs)).
%%   {ok, Child} = ,
%%   Ch = supervisor:which_children(?MODULE),
%%   case lists:keytake(Id, 1, Ch) of
%%      {value, {Id, Pid, _T, _How}, _TupleList2} -> {ok, Pid};
%%      _ -> erlang:error(graph_not_started, [Id])
%%   end.
%%%===================================================================
%%% Internal functions
%%%===================================================================
child(Id, Params) ->
   {Id, {  df_graph, start_link, [Id, Params]},
      temporary, 5000, worker, dynamic}.
