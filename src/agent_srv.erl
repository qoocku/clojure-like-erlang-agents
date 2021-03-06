%%%
%%% @doc The OTP compliant agent process implementation.
%%% Uses {@std_doc gen_server.html gen_server} framework.
%%% @author Damian T. Dobroczy\\'nski
%%% @since 2011-11-15
%%% @headerfile "state.hrl"
%%%
-module (agent_srv).
-author ("Damian T. Dobroczy\\\\'nski <qoocku@gmail.com>").
-behavior (gen_server).
-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3,
          send_off/3,
          next/2,
          next/3]).

-include ("state.hrl").

-type state() :: #state{}.
-opaque ref() :: {gen, pid()}. %% The implementation process reference type.
-export_type ([ref/0]).

-define (REF(Pid), {gen, Pid}).
-define (SELF, ?REF(self())).

%% @private
%% @doc Initialises the agent's process.
%% See {@std_doc gen_server.html#Module:init-1 gen_server `init' callback}
%% 
-spec init ({any(), agent:validation_fun(), agent:timeout_fun()}) -> {ok, state()}.

init ({Value0, VFun, TFun}) -> 
  {ok, #state{value = Value0,
              vfun  = VFun,
              tfun  = TFun}};
init (Value0) ->
  init({Value0, fun (_) -> true end, fun (_,V) -> {ok, V} end}).

%% @private
%% @doc Handles implementation internal synchronous messages.
%% See {@std_doc gen_server.html#Module:handle_call-3 gen_server `handle_call' callback}
%% 
-spec handle_call (state      |
                   validation |
                   timeout    |
                   shutdown, term(), state()) ->
                      {reply, {ok, any()}, state()} | {stop, normal, state()}.

handle_call (state, _From, State = #state{value = V}) ->
  {reply, {ok, V}, State};
handle_call (validation, _From, State = #state{vfun = V}) ->
  {reply, {ok, V}, State};
handle_call (timeout, _From, State = #state{tfun = V}) ->
  {reply, {ok, V}, State};
handle_call (shutdown, From, State) ->
  gen_server:reply(From, ok),
  {stop, normal, State}.

%% @private
%% @doc Handles implementation internal asynchronous messages.
%% See {@std_doc gen_server.html#Module:handle_cast-2 gen_server `handle_cast' callback}
%% 
-spec handle_cast({add_watch, agent:watch_fun()}      |
                  {'$next', agent:callback_result(), agent:value_vsn()}  |
                  shutdown                            |
                  {validation, agent:validation_fun()}|
                  {timeout, agent:timeout_fun()}      |
                  {send, agent:agent_fun()}           |
                  {send_off, agent:agent_fun()}, state()) -> {noreply, state()} | 
                                                             {noreply, state(), timeout()} |
                                                             {stop, normal | {invalid_state_value, any()}, state()}.
                                    
handle_cast ({add_watch, Fun}, State) ->
  Pid = agent_value_watch:new({gen, self()}, Fun),
  erlang:monitor(process, Pid),
  {noreply, State};
handle_cast ({'$next', Next, undefined, _}, State) ->
  ?MODULE:next(Next, State);
handle_cast ({'$next', Next, Vsn, OffFun}, State) ->
  ?MODULE:next(Next, State, {Vsn, OffFun});
handle_cast (shutdown, State) ->
  {stop, normal, State};
handle_cast ({validation, VFun}, State) ->
  {noreply, State#state{vfun = VFun}};
handle_cast ({timeout, TFun}, State) ->
  {noreply, State#state{tfun = TFun}};
handle_cast ({send, Fun}, State = #state{value = V}) ->
  ?MODULE:next(Fun(?SELF, V), State);
handle_cast ({send_off, Fun, Versioned}, State) ->
  ?MODULE:send_off(Fun, State, Versioned).

%% @private
%% @doc Handles implementation internal non-gen_server messages.
%% The messages comes from timer and watchers processes.
%% See {@std_doc gen_server.html#Module:handle_info-2 gen_server `handle_info' callback}
%% 
-spec handle_info ({'DOWN', any(), process, pid(), any()} |
                   timeout, state()) -> {noreply, state()} | 
                                        {noreply, state(), timeout()} |
                                        {stop, {invalid_state_value, any()}, state()}.

handle_info ({'DOWN', _, process, Pid, Info}, State) ->
  error_logger:error_report([{?MODULE, monitor_message},
                             {what, probably_watcher_down},
                             {reason, Info},
                             {watcher_pid, Pid},
                             {agent, {gen, self()}},
                             {agent_pid, self()}]),
  {noreply, State};
handle_info (timeout, State = #state{tfun = Fun, value = V}) ->
  ?MODULE:next(Fun(?SELF, V), State).

%% @private
%% @doc Terminates the agent's process.
%% So far does nothing.
%% See {@std_doc gen_server.html#Module:terminate-2 gen_server `terminate' callback}
%% 
-spec terminate (normal | {invalid_state_value, any()}, state()) -> any().

terminate (_Reason, _State) ->
  ok.

%% @private
%% @doc Agent's code evolution.
%% See {@std_doc gen_server.html#Module:code_change-3 gen_server `code_change' callback}
%% 
-spec code_change (any(), state(), any()) -> {ok, state()}.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%% @hidden 
-spec next (agent:callback_result(),
            state()) -> {noreply, state()} |
                        {noreply, state(), timeout()} |
                        {stop, {invalid_state_value, any()}, state()}.

next (Next, State) ->
  next(Next, State, undefined).

-compile ({inline, [next/2]}).

%% @hidden
-spec next (agent:callback_result(),
            state(),
            {agent:value_vsn(), agent:agent_fun()} | undefined) -> {noreply, state()} |
                                                                   {noreply, state(), timeout()} |
                                                                   {stop, {invalid_state_value, any()}, state()}.

next (Next, State, undefined) -> %% ignore the version
  next_aux(Next, State);
next (Next, State = #state{vsn = Vsn0}, {Vsn0, _}) -> %% no change in version
  next_aux(Next, State);
next (_Next, State, {_, OffFun}) -> %% change in version
  error_logger:info_report([{module, ?MODULE},
                            {where, shift_state},
                            {what, transaction_retry},
                            {transaction, OffFun},
                            {agent, self()}]),
  send_off(OffFun, State, true).

%% @hidden
-spec next_aux (agent:callback_result(), state()) -> {noreply, state(), timeout()} |
                                                     {stop, {invalid_state_value, any()}, state()}.

next_aux (Next, State = #state{value =V, vfun = Fun}) ->
  {ok, NV, Timeout} = case Next of
                        {ok, NV0} -> {ok, NV0, infinity};
                        Other     -> Other
                      end,
  case Fun(NV) of
    true ->
      agent_value_watch:notify_watchers(?SELF, V, NV),
      {noreply, State#state{value = NV, vsn = erlang:make_ref()}, Timeout};
    false ->
      {stop, {invalid_state_value, NV}, State}
  end.


%% @private
%% @doc Starts send-off process.
%%
-spec send_off (agent:agent_fun(), state(), boolean()) -> {noreply, state()}.

send_off (Fun, 
          State = #state{value = V, vsn = Vsn}, 
          Versioned) ->
  Self = self(),
  erlang:spawn_link(fun () ->
                        Next = Fun(?REF(Self), V),
                        gen_server:cast(Self, {'$next', Next, case Versioned of
                                                                true  -> Vsn;
                                                                false -> undefined
                                                              end, Fun})
                    end),
  {noreply, State}.
