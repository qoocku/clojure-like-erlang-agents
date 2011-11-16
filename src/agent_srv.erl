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
          send_off/2,
          next/2]).

-include ("state.hrl").

-type state() :: #state{}.
-opaque ref() :: {gen, pid()}. %% The implementation process reference type.
-export_types ([ref/0]).

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
              tfun  = TFun}}.

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
                  {'$next', agent:callback_result()}  |
                  shutdown                            |
                  {validation, agent:validation_fun()}|
                  {timeout, agent:timeout_fun()}      |
                  {send, agent:agent_fun()}           |
                  {send_off, agent:agent_fun()}, state()) -> {noreply, state()} | 
                                                             {stop, normal | {invalid_state_value, any()}}.
                                    
handle_cast ({add_watch, Fun}, State) ->
  Pid = agent_value_watch:new({gen, self()}, Fun),
  erlang:monitor(process, Pid),
  {noreply, State};
handle_cast ({'$next', Next}, State) ->
  ?MODULE:next(Next, State);
handle_cast (shutdown, State) ->
  {stop, normal, State};
handle_cast ({validation, VFun}, State) ->
  {noreply, State#state{vfun = VFun}};
handle_cast ({timeout, TFun}, State) ->
  {noreply, State#state{tfun = TFun}};
handle_cast ({send, Fun}, State = #state{value = V}) ->
  ?MODULE:next(Fun(?SELF, V), State);
handle_cast ({send_off, Fun}, State) ->
  ?MODULE:send_off(Fun, State).

%% @private
%% @doc Handles implementation internal non-gen_server messages.
%% The messages comes from timer and watchers processes.
%% See {@std_doc gen_server.html#Module:handle_info-2 gen_server `handle_info' callback}
%% 
-spec handle_info ({'DOWN', any(), process, pid(), any()} |
                   timeout, state()) -> {noreply, state()} | {stop, {invalid_state_value, any()}, state()}.

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
-spec next (agent:callback_result(), state()) -> {noreply, state()} | {stop, {invalid_state_value, any()}}.

next ({ok, NV}, State = #state{value = V, vfun = Fun}) ->
  case Fun(NV) of
    true ->
      notify_watchers(V, NV),
      {noreply, State#state{value = NV}};
    false ->
      {stop, {invalid_state_value, NV}, State}
  end;
next ({ok, NV, Timeout}, State = #state{value = V, vfun = Fun}) ->
  case Fun(NV) of
    true ->
      notify_watchers(V, NV),
      {noreply, State#state{value = NV}, Timeout};
    false ->
      {stop, {invalid_state_value, NV}, State}
  end.

%% @private
%% @doc Starts send-off process.
%%
-spec send_off (agent:agent_fun(), state()) -> {noreply, state()}.

send_off (Fun, State = #state{value = V}) ->
  Self = self(),
  erlang:spawn(fun () ->
                   Next = Fun(?REF(Self), V),
                   gen_server:cast(Self, {'$next', Next})
               end),
  {noreply, State}.

%% @private
%% @doc Notify agent's watchers that the state value has been set.
%% @see agent_proc:notify_watchers/2

-spec notify_watchers (any(), any()) -> any().

notify_watchers (OldVal, NewVal) ->
  {monitors, Monitors} = erlang:process_info(self(), monitors),
  lists:foreach(fun ({process, Watcher}) ->
                    Watcher ! {'agent-value', ?SELF, OldVal, NewVal}
                end, Monitors).
