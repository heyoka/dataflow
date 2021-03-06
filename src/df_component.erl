%% Date: 28.12.16 - 18:17
%% Ⓒ 2016 heyoka
-module(df_component).
-author("Alexander Minichmair").

-include("dataflow.hrl").

-behaviour(gen_server).
-behavior(df_graph_node).

%% API
-export([start_link/5]).
-export([start_node/4, inports/1, outports/1]).

%% Callback API
%%-export([request_items/2, emit/1, emit/2]).

%% gen_server callbacks
-export([init/1,
   handle_call/3,
   handle_cast/2,
   handle_info/2,
   terminate/2,
   code_change/3]).


-type cbstate()         :: term().

-type auto_request()    :: 'all' | 'emit' | 'none'.

-type df_port()         :: non_neg_integer().


-record(state, {
   flow_mode = push     :: push | pull,
   node_id              :: term(), %% this nodes id
   component            :: atom(), %% callbacks module name
   cb_state             :: cbstate(), %% state for callback
   cb_handle_info       :: true | false,
   inports              :: list(), %% list of inputs {port, pid}
   subscriptions        :: list(#subscription{}),
   auto_request         :: none | all | emit,
   history              :: list(),
   emitted = 0          :: non_neg_integer()

}).


%%%===================================================================
%%% CALLBACKS
%%%===================================================================

%% @doc
%% INIT/3
%%
%% initialisation
%%
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
%% {emit, Port :: port(), Value :: term(), cbstate()} :: used emit right after processing
%%
%% :: request a value in return of a process call
%% {request, ReqPort :: port(), ReqPid :: pid(), cbstate()}
%%
%% :: emit and request a value
%% {emit_request, OutPort :: port(), Value :: term(), ReqPort :: port(), ReqPid :: pid(), cbstate()}
%%
%%
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
    {Name :: atom(), Type :: dataflow:option_value(), Default :: dataflow:option_value()} |
    {Name :: atom(), Type :: atom()}
 ).
%% @end


%% @doc
%% INPORTS/0
%%
%% optional
%% provide a list of inports for the component :
%%
%% -callback inports()  -> {ok, list()}.
%%
%%



%% @doc
%% OUTPORTS/0
%%
%% optional
%% provide a list of outports for the component:
%%
%% -callback outports() -> {ok, list()}.
%%
%%


%% @doc
%% HANDLE_INFO/2
%%
%% optional
%% handle other messages that will be sent to this process :
%%
%% -callback handle_info(Request :: term(), State :: cbstate())
%% -> {ok, NewCallbackState :: cbstate()} | {error, Reason :: term()}.
%% @end


%% @doc
%% SHUTDOWN/1
%%
%% optional
%% called when component process is about to stop :
%%
%% -callback shutdown(State :: cbstate())
%% -> any().
%% @end

%%-optional_callbacks([inports/0, outports/0, handle_info/2, shutdown/1]). %% erlang 18+



%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(atom(), term(), list(), list(), term()) ->
   {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Component, NodeId, Inports, Outports, Args) ->
   gen_server:start_link(?MODULE, [Component, NodeId, Inports, Outports, Args], []).

start_node(Server, Inputs, Subscriptions, FlowMode) ->
   gen_server:call(Server, {start, Inputs, Subscriptions, FlowMode}).

inports(Module) ->
   case erlang:function_exported(Module, inports, 0) of
      true -> Module:inports();
      false -> inports()
   end.

outports(Module) ->
   case erlang:function_exported(Module, outports, 0) of
      true -> Module:outports();
      false -> outports()
   end.


%%%==========================================================
%%% Callback API
%%%
%%% these are exposed through the dataflow module now !
%%%==========================================================
%%request_items(Port, PublisherPids) when is_list(PublisherPids) ->
%%   [Pid ! {request, self(), Port} || Pid <- PublisherPids].
%%
%%emit(Value) ->
%%   emit(1, Value).
%%emit(Port, Value) ->
%%   erlang:send_after(0, self(), {emit, {Port, Value}}).


%%% %%%%%%%%%%%%%%%%
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
   {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
   {stop, Reason :: term()} | ignore).
init([Component, NodeId, Inports, _Outports, Args]) ->
   {module, Component} = code:ensure_loaded(Component),
   ?LOG("init component ~p",[Component]),
   InputPorts = lists:map(fun({_Pid, Port}) -> Port end, Inports),
   {ok, #state{component = Component, node_id = NodeId, subscriptions = [],
      inports = InputPorts, cb_state = Args}}.


handle_call({start, Inputs, Subscriptions, FlowMode}, _From,
    State=#state{component = CB, cb_state = CBState, node_id = NId}) ->

   gen_event:notify(dfevent_component, {start, State#state.node_id, FlowMode}),

   {ok, AutoRequest, NewCBState} = CB:init(NId, Inputs, dataflow:build_options(CB, CBState)),

   AR = case FlowMode of pull -> AutoRequest; push -> none end,
   CallbackHandlesInfo = erlang:function_exported(CB, handle_info, 2),
   {reply, ok,
      State#state{
         subscriptions = Subscriptions,
         inports = Inputs,
         auto_request = AR,
         cb_state = NewCBState,
         flow_mode = FlowMode,
         cb_handle_info = CallbackHandlesInfo}}
;
handle_call(_What, _From, State) ->
   {reply, State}
.

handle_cast(_Request, State) ->
   {noreply, State}.


%% @doc
%% these are the messages from and to other dataflow nodes
%% do not use these tags in your callback 'handle_info' functions :
%% 'reqeust' | 'item' | 'emit' | 'pull' | 'stop'
%% you will not receive the info message in the callback
%%
%% @end
handle_info({request, ReqPid, ReqPort}, State=#state{subscriptions = Ss}) ->
   NewSubs = df_subscription:request(Ss, ReqPid, ReqPort),
   {noreply, State#state{subscriptions =  NewSubs}};

handle_info({item, {Inport, Value}}, State=#state{cb_state = CBState, component = Module,
   subscriptions = Subs, flow_mode = FMode, auto_request = AR}) ->

   gen_event:notify(dfevent_component, {item, State#state.node_id, {Inport, Value}}),
   {NewState, Requested, REmitted} =
      case Module:process(Inport, Value, CBState) of

         {emit, {Port, Emitted}, NState} ->
            gen_event:notify(dfevent_component, {emitting, State#state.node_id, {Port, Emitted}}),
            NewSubs = df_subscription:output(Subs, Emitted, Port),
%%            ?LOG("Node ~p is emitting: ~p",[State#state.node_id, {Port, Emitted}]),
            {State#state{subscriptions = NewSubs, cb_state = NState},
               false, true};
         {emit, Emitted, NState} ->
            gen_event:notify(dfevent_component, {emitting, State#state.node_id, {1, Emitted}}),
            NewSubs = df_subscription:output(Subs, Emitted, 1),
            {State#state{subscriptions = NewSubs, cb_state = NState},
               false, true};
         {request, {Port, PPids}, NState} when is_list(PPids) ->
            gen_event:notify(dfevent_component, {requests, State#state.node_id, Port}),
            maybe_request_items(Port, PPids, FMode),
            {State#state{cb_state = NState},
               true, false};
         {emit_request, {Port, Emitted}, {ReqPort, PPids}, NState} when is_list(PPids) ->
            gen_event:notify(dfevent_component, {emitting_requests, State#state.node_id, {Port, Emitted}}),
            NewSubs = df_subscription:output(Subs, Emitted, Port),
            maybe_request_items(ReqPort, PPids, FMode),
            ?LOG("Node ~p is emitting: ~p",[State#state.node_id, {Port, Emitted}]),
            {State#state{subscriptions = NewSubs, cb_state = NState},
               true, false};
         {emit_request, Emitted, {ReqPort, PPids}, NState} when is_list(PPids) ->
            gen_event:notify(dfevent_component, {emitting_requests, State#state.node_id, {1, Emitted}}),
            NewSubs = df_subscription:output(Subs, Emitted, 1),
            maybe_request_items(ReqPort, PPids, FMode),
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
      flow_mode = FMode, auto_request = AR, emitted = EmitCount}) ->

   gen_event:notify(dfevent_component, {emitting, State#state.node_id, {Outport, Value}}),

   NewSubs = df_subscription:output(Ss, Value, Outport),
   NewState = State#state{subscriptions = NewSubs},
   case AR of
      none  -> ok;
      _     -> request_all(State#state.inports, FMode)
   end,
   {noreply, NewState#state{emitted = EmitCount+1}};

handle_info(pull, State=#state{inports = Ins}) ->
   lists:foreach(fun({Port, Pid}) -> dataflow:request_items(Port, [Pid]) end, Ins),
   {noreply, State}
;
handle_info(stop, State=#state{node_id = N, component = Mod, cb_state = CBState}) ->
   gen_event:notify(dfevent_component, {stopping, N, Mod}),
   case erlang:function_exported(Mod, shutdown, 1) of
      true -> Mod:shutdown(CBState);
      false -> ok
   end,
   {stop, normal, State}
;
handle_info(Req, State=#state{component = Module, cb_state = CB, cb_handle_info = true}) ->
   NewCB = case Module:handle_info(Req, CB) of
              {ok, CB0} -> CB0;
              {error, _Reason} -> error
           end,
   {noreply, State#state{cb_state = NewCB}}
;
handle_info(_Req, State=#state{cb_handle_info = false}) ->
   {noreply, State}
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
   dataflow:request_items(Port, Pids).


%%%===================================================================
%%% PORTS for modules
%%%

inports() ->
   [{1, nil}].

outports() ->
   inports().