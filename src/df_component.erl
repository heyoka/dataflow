%% Date: 28.12.16 - 18:17
%% Ⓒ 2016 heyoka
-module(df_component).
-author("Alexander Minichmair").

-include("dataflow.hrl").

-behaviour(gen_server).
-behavior(df_graph_node).

%% API
-export([start_link/5, inports/0, outports/0]).
-export([start_node/4]).

%% Callback API
-export([request_items/2, emit/2]).

%% gen_server callbacks
-export([init/1,
   handle_call/3,
   handle_cast/2,
   handle_info/2,
   terminate/2,
   code_change/3]).

-record(state, {
   flow_mode = push     :: push | pull,
   component            :: atom(), %% callbacks module name
   node_id              :: term(), %% this nodes id
   inports              :: list(), %% list of inputs {port, pid}
   outports             :: list(),  %% list of outputs {port, pid}
   cb_state             :: cbstate(), %% state for callback
   subscriptions        :: list(#subscription{}),
   auto_request         :: none | all | emit

}).


%%%===================================================================
%%% CALLBACKS
%%%===================================================================
-type cbstate()      :: term().

-type auto_request() :: 'all' | 'emit' | 'none'.

-type df_port()      :: non_neg_integer().


%% @doc
%% initialisation
%% @end
-callback init(NodeId :: term(), Inputs :: list(), Args :: term())
       -> {ok, auto_request(), cbstate()}.


%% @doc
%% provide a list of inports for the component
%% @end
-callback inports()  -> {ok, list()}.

%% @doc
%% provide a list of outports for the component
%% @end
-callback outports() -> {ok, list()}.

%% @doc
%% process value or batch incoming on specific inport
%%
%% return values :
%%
%% :: just return the state
%% {ok, cbstate()}
%%
%% :: used to emit a value right after processing
%% {emit, Port :: port(), Value :: term(), cbstate()} :: used emit right after processing
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

%% @doc
%% handle other messages that will be sent to this process
%% @end
-callback handle_info(Request :: term(), State :: cbstate())
      -> {ok, NewCallbackState :: cbstate()} | {error, Reason :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(atom(), term(), list(), list(), term()) ->
   {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Component, NodeId, Inports, Outports, Args) ->
   gen_server:start_link(?MODULE, [Component, NodeId, Inports, Outports, Args], []).

start_node(Server, Inputs, Subscriptions, FlowMode) ->
   gen_server:call(Server, {start, Inputs, Subscriptions, FlowMode}).

%%% Callback API %%%
request_items(Port, PublisherPids) when is_list(PublisherPids) ->
   [Pid ! {request, self(), Port} || Pid <- PublisherPids].

emit(Port, Value) ->
   erlang:send_after(0, self(), {emit, {Port, Value}}).

%%% %%%%%%%%%%%%%%%%
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
   {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term()} | ignore).
init([Component, NodeId, Inports, Outports, Args]) ->
   InputPorts = lists:map(fun({_Pid, Port}) -> Port end, Inports),
   {ok, #state{component = Component, node_id = NodeId, subscriptions = [],
      outports = Outports, inports = InputPorts, cb_state = Args}}.


-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
   {reply, Reply :: term(), NewState :: #state{}} |
   {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_call({start, Inputs, Subscriptions, FlowMode}, _From,
    State=#state{component = CB, cb_state = CBState, node_id = NId}) ->
%%   io:format("~p | ~p 'start' with : ~p ~n", [State#state.node_id, self(), {Inputs, Subscriptions}]),
   {ok, AutoRequest, NewCBState} = CB:init(NId, Inputs, CBState),

   AR = case FlowMode of pull -> AutoRequest; push -> none end,

   {reply, ok, State#state{
      subscriptions = Subscriptions, inports = Inputs, auto_request = AR,
      cb_state = NewCBState, flow_mode = FlowMode}}
;
handle_call({inports}, _From, State=#state{component = Module}) ->
   Res = Module:inports(),
   {reply, {ok, Res}, State}
;
handle_call({outports}, _From, State=#state{component = Module}) ->
   Res = Module:outports(),
   {reply, {ok, Res}, State}.


-spec(handle_cast(Request :: term(), State :: #state{}) ->
   {noreply, NewState :: #state{}} |
   {noreply, NewState :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
   {noreply, State}.



handle_info({request, ReqPid, ReqPort}, State=#state{subscriptions = Ss}) ->
   io:format("Node ~p requests item: ~p~n",[ReqPid, {ReqPid, ReqPort}]),
   NewSubs = df_subscription:request(Ss, ReqPid, ReqPort),
   {noreply, State#state{subscriptions =  NewSubs}};

handle_info({item, {Inport, Value}}, State=#state{cb_state = CBState, component = Module,
   subscriptions = Subs, flow_mode = FMode, auto_request = AR}) ->

   io:format("Node ~p got item: ~p~n",[State#state.node_id, {Inport, Value}]),
   {NewState, Requested, REmitted} =
      case Module:process(Inport, Value, CBState) of
         {emit, {Port, Emitted}, NState} ->
            NewSubs = df_subscription:output(Subs, Emitted, Port),
            io:format("Node ~p is emitting: ~p~n",[State#state.node_id, {Port, Emitted}]),
            {State#state{subscriptions = NewSubs, cb_state = NState},
               false, true};
         {request, {Port, PPids}, NState} when is_list(PPids) ->
            maybe_request_items(Port, PPids, FMode),
            {State#state{cb_state = NState},
               true, false};
         {emit_request, {Port, Emitted}, {ReqPort, PPids}, NState} when is_list(PPids)->
            NewSubs = df_subscription:output(Subs, Emitted, Port),
            maybe_request_items(ReqPort, PPids, FMode),
            io:format("Node ~p is emitting: ~p~n",[State#state.node_id, {Port, Emitted}]),
            {State#state{subscriptions = NewSubs, cb_state = NState},
               true, false};
         {ok, NewCBState} ->
            {State#state{cb_state = NewCBState},
               false, false};
         {error, _What} ->
            exit(_What)
      end,
   %% if requested already, no auto-request will happen
   case FMode == pull of
      true -> case {Requested, AR, REmitted} of
                 {true, _, _} -> ok;
                 {false, none, _} -> ok;
                 {false, emit, false} -> ok;
                 {false, emit, true} -> request_all(State#state.inports, FMode);
                 {false, all, _} -> request_all(State#state.inports, FMode)
              end;
      false -> ok
   end
   ,

   {noreply, NewState};

handle_info({emit, {Outport, Value}}, State=#state{subscriptions = Ss,
      flow_mode = FMode, auto_request = AR}) ->

   io:format("Node ~p is emitting: ~p~n",[State#state.node_id, {Outport, Value}]),
   NewSubs = df_subscription:output(Ss, Value, Outport),
   NewState = State#state{subscriptions = NewSubs},
   case AR of
      none  -> ok;
      _     -> request_all(State#state.inports, FMode)
   end,
   {noreply, NewState};

handle_info(pull, State=#state{inports = Ins}) ->
   lists:foreach(fun({Port, Pid}) -> request_items(Port, [Pid]) end, Ins),
   {noreply, State}
;

handle_info(Req, State=#state{component = Module, cb_state = CB}) ->
   NewCB = case Module:handle_info(Req, CB) of
              {noreply, CB0} -> CB0;
              {error, _Reason} -> error
           end,
   {noreply, State#state{cb_state = NewCB}}
;
handle_info(stop, State) ->
   {stop, normal, State}
.

%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
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

request_all(_Inports, push) ->
   ok;
request_all(Inports, pull) ->
   {_Ports, Pids} = lists:unzip(Inports),
   maybe_request_items(all, Pids, pull).
maybe_request_items(_Port, _Pids, push) ->
   ok;
maybe_request_items(Port, Pids, pull) ->
   request_items(Port, Pids).

inports() ->
   erlang:error(not_implemented).

outports() ->
   erlang:error(not_implemented).