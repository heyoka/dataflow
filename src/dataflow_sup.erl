%% Date: 16.04.17 - 22:13
%% Ⓒ 2017 Alexander Minichmair
-module(dataflow_sup).
-author("Alexander Minichmair").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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
   supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
-spec(init(Args :: term()) ->
   {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
      MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
      [ChildSpec :: supervisor:child_spec()]
   }} |
   ignore |
   {error, Reason :: term()}).
init([]) ->
   RestartStrategy = one_for_one,
   MaxRestarts = 10,
   MaxSecondsBetweenRestarts = 3600,
   Procs = [
      {df_events,
         {gen_event, start_link, [{local, df_events}]},
         permanent, 5000, worker, dynamic},

      {graph_sup,
         {graph_sup, start_link, []},
         permanent, infinity, supervisor, [graph_sup]}
   ],

   SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
   {ok, {SupFlags, Procs}}.
