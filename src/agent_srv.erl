-module (agent_srv).
-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3,
          send_off/2,
          next/2]).

-include ("state.hrl").

-type ref() :: {gen, pid()}.
-export_types ([ref/0]).

-define (REF(Pid), {gen, Pid}).
-define (SELF, ?REF(self())).

init ({Value0, VFun, TFun}) -> 
  {ok, #state{value = Value0,
              vfun  = VFun,
              tfun  = TFun}}.

handle_call (state, _From, State = #state{value = V}) ->
  {reply, {ok, V}, State};
handle_call (validation, _From, State = #state{vfun = V}) ->
  {reply, {ok, V}, State};
handle_call (timeout, _From, State = #state{tfun = V}) ->
  {reply, {ok, V}, State};
handle_call (shutdown, From, State) ->
  gen_server:reply(From, ok),
  {stop, normal, State}.

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

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

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

send_off (Fun, State = #state{value = V}) ->
  Self = self(),
  erlang:spawn(fun () ->
                   Next = Fun(?REF(Self), V),
                   gen_server:cast(Self, {'$next', Next})
               end),
  {noreply, State}.

notify_watchers (OldVal, NewVal) ->
  {monitors, Monitors} = erlang:process_info(self(), monitors),
  lists:foreach(fun ({process, Watcher}) ->
                    Watcher ! {'agent-value', ?SELF, OldVal, NewVal}
                end, Monitors).
