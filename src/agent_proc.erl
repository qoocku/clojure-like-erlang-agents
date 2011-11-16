%%%
%%% @doc An implementation of agent process not to be supervised (OTP non-compliant).
%%%
-module (agent_proc).
-export ([start/1,
          send_off/3,
          init/1,
          loop/1]).

-include ("state.hrl").
-include_lib ("eunit/include/eunit.hrl").

start ([State0]) ->
  start([State0, fun (_) -> true end, fun (_, V) -> {ok, V} end]);
start ([State0, VFun, TFun]) ->
  {ok, erlang:spawn(?MODULE, init, [#state{value = State0,
                                           vfun  = VFun,
                                           tfun  = TFun}])}.

init (State) ->
  loop(State).

loop (stop) ->
  ok;
loop (State = #state{value = Value,
                     tfun  = TFun,
                     vfun  = VFun}) ->
  Self = self(),
  Next = receive
           {'DOWN', _, process, Watcher, Info} ->
             error_logger:error_report([{?MODULE, monitor_message},
                                        {what, probably_watcher_down},
                                        {reason, Info},
                                        {watcher_pid, Watcher},
                                        {agent, self()}]),
             none;
           {'$next', Next1} ->
             Next1;
           {add_watch, Fun} ->
             Watcher = agent_value_watch:new(self(), Fun),
             erlang:monitor(process, Watcher),
             none;
           {Pid, state} when is_pid(Pid) ->
             Pid ! {Self, state, Value},
             none;
           {Pid, validation} when is_pid(Pid) ->
             Pid ! {Self, validation, VFun},
             none;
           {Pid, timeout} when is_pid(Pid) ->
             Pid ! {Self, timeout, TFun},
             none;
           {validation, Fun} when is_function(Fun) ->
             {vfun, Fun};
           {timeout, Fun} when is_function(Fun) ->
             {tfun, Fun};
           timeout ->
             eval(timeout, Self, TFun, Value);
           {send, Fun} when is_function(Fun) ->
             eval(send, Self, Fun, Value);
           {send_off, Fun} when is_function(Fun) ->
             Self = self(),
             erlang:spawn(?MODULE, send_off, [Self, Value, Fun]),
             none;
           shutdown ->
             stop;
           {Pid, shutdown} when is_pid(Pid) ->
             Pid ! {Self, ok},
             stop
         end,
  ?MODULE:loop(next(Next, State)).

error_report (Context, E, R, Pid) ->
  error_report(Context, E, R, Pid, []).

error_report (Context, E, R, Pid, Rest) ->
  error_logger:error_report([{module, ?MODULE},
                             {where, Context},
                             {what, {E,R}},
                             {self, self()},
                             {agent, Pid}] ++ Rest).


eval (validation, Pid, Fun, V) ->
  try Fun(V) of
      Value -> Value
  catch
    E:R ->
      error_report(validation, E, R, Pid, [{function, Fun},{argument, V}]),
      false
  end;
eval (Context, Pid, Fun, V) ->
  try Fun(Pid, V) of
      Next -> Next
  catch
    E:R ->
      error_report(Context, E, R, Pid, [{function, Fun},{argument, V}]),
      case Context of
        send_off -> {ok, V};
        _Other   -> stop
      end                      
  end.
      
next (none, State) ->
  State;
next (stop, _State) ->
  stop;
next ({vfun, Fun}, State = #state{value = V}) ->
  S1 = next({ok, V}, State),
  S1#state{vfun = Fun};
next ({tfun, Fun}, State) ->
  State#state{tfun = Fun};
next ({ok, NewValue}, State = #state{value = V, vfun = VFun}) ->
  case eval(validation, self(), VFun, NewValue) of
    true ->
      notify_watchers(V, NewValue),
      State#state{value = NewValue};
    false ->
      exit({invalid_state_value, NewValue})
  end;
next ({ok, NewValue, Timeout}, State = #state{value = V, vfun = VFun}) ->
  case eval(validation, self(), VFun, NewValue) of
    true ->
      erlang:send_after(Timeout, self(), timeout),
      notify_watchers(V, NewValue),
      State#state{value = NewValue};
    false ->
      exit({invalid_state_value, NewValue})
  end;
next (Other, _State) ->
  exit({invalid_state_shift_value, Other}).

send_off (Self, Value, Fun) ->
  Next = eval(send_off, Self, Fun, Value),
  Self ! {'$next', Next}.

notify_watchers (OldVal, NewVal) ->
  {monitors, Monitors} = erlang:process_info(self(), monitors),
  lists:foreach(fun ({process, Watcher}) ->
                    Watcher ! {'agent-value', self(), OldVal, NewVal}
                end, Monitors).
