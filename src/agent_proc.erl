%%%
%%% @doc An implementation of agent process not to be supervised (OTP non-compliant).
%%% @author Damian T. Dobroczy\\'nski
%%% @since 2011-11-15
%%% @headerfile "state.hrl"
%%%
-module (agent_proc).
-author ("Damian T. Dobroczy\\\\'nski <qoocku@gmail.com>").
-export ([start/1,
          send_off/4,
          init/1,
          loop/1]).

-include ("state.hrl").

-type state() :: #state{}. %% The internal agent process state.

%%
%% @doc Starts simple agent process.
%% The arguments lsit must contain `State0' value and optionaly
%% validation and timeout function.
%% The agent has initial state value set to `State0'.
%%
-spec start ([any() | agent:validation_fun() | agent:timeout_fun()]) -> {ok, pid()}.

start ([State0]) ->
  start([State0, fun (_) -> true end, fun (_, V) -> {ok, V} end]);
start ([State0, VFun, TFun]) ->
  {ok, erlang:spawn(?MODULE, init, [#state{value = State0,
                                           vfun  = VFun,
                                           tfun  = TFun}])}.

%% @private
%% @doc Process initialisation.
%% 
-spec init (state()) -> no_return().

init (State) ->
  loop(State#state{vsn = erlang:make_ref()}).

%% @private
%% @doc Agent process main loop.
%%
-spec loop (stop | state()) -> no_return().

loop (stop) ->
  ok;
loop (State = #state{value = Value,
                     tfun  = TFun,
                     vfun  = VFun,
                     vsn   = Vsn}) ->
  Self = self(),
  Next = receive
           {'DOWN', _, process, Watcher, Info} ->
             error_logger:error_report([{?MODULE, monitor_message},
                                        {what, probably_watcher_down},
                                        {reason, Info},
                                        {watcher_pid, Watcher},
                                        {agent, self()}]),
             none;
           {'$next', Next1, {undefined, _}} ->
             Next1;
           {'$next', Next1, {Vsn, _}} ->
             Next1;
           {'$next', _Next1, {_Vsn0, OffFun}} ->
             spawn_send_off(Vsn, Value, OffFun),
             none;
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
             eval(timeout, Self, TFun, Value, Vsn);
           {send, Fun} when is_function(Fun) ->
             eval(send, Self, Fun, Value, Vsn);
           {send_off, Fun, Bool} when is_function(Fun) ->
             spawn_send_off(case Bool of
                              true -> Vsn;
                              false -> undefined
                            end, Value, Fun),
             none;
           shutdown ->
             stop;
           {Pid, shutdown} when is_pid(Pid) ->
             Pid ! {Self, ok},
             stop
         end,
  ?MODULE:loop(next(Next, State)).

%% @private
%% @doc Logs error.
%% Error `E' is logged with given `Context', reason `R', agent pid `Pid' and
%% rest of report entries list in `Rest'.
%%
-spec error_report (validation | timeout | send | send_off,
                    atom(), term(), pid(), list()) -> any().
                       
error_report (Context, E, R, Pid, Rest) ->
  error_logger:error_report([{module, ?MODULE},
                             {where, Context},
                             {what, {E,R}},
                             {self, self()},
                             {agent, Pid}] ++ Rest).

%% @private
%% @doc Evaluates agent function.
%% If an exception is caught, the error is logged, and specific value is
%% returned so that the agent stops it's loop.
%%
-spec eval (validation | timeout | send | send_off,
            pid(), 
            agent:validation_fun() |
            agent:timeout_fun()    |
            agent:agent_fun(),
            any(), agent:value_vsn()) -> agent:callback_result() | boolean().
               
eval (validation, Pid, Fun, V, Vsn) ->
  try Fun(V) of
      Value -> Value
  catch
    E:R ->
      error_report(validation, E, R, Pid, [{function, Fun},{argument, V},{vsn, Vsn}]),
      false
  end;
eval (Context, Pid, Fun, V, Vsn) ->
  try Fun(Pid, V) of
      Next -> Next
  catch
    E:R ->
      error_report(Context, E, R, Pid, [{function, Fun},{argument, V},{vsn, Vsn}]),
      case Context of
        send_off -> {ok, V};
        _Other   -> stop
      end                      
  end.
  
%% @private
%% @doc Shifts the agent to the next state.
%% The next state may be loop continuation with the new state value or
%% exiting the message loop.
%%
-spec next (Change::(none | 
                     stop | 
                     {vfun, agent:validation_fun()} | 
                     {tfun, agent:timeout_fun()}    |
                     agent:callback_result()        |
                     any()), state()) -> state() | stop.

next (none, State) ->
  State;
next (stop, _State) ->
  stop;
next ({vfun, Fun}, State = #state{value = V}) ->
  S1 = next({ok, V}, State),
  S1#state{vfun = Fun};
next ({tfun, Fun}, State) ->
  State#state{tfun = Fun};
next ({ok, NewValue}, State = #state{value = V, vfun = VFun, vsn = Vsn}) ->
  case eval(validation, self(), VFun, NewValue, Vsn) of
    true ->
      notify_watchers(V, NewValue),
      State#state{value = NewValue, vsn = erlang:make_ref()};
    false ->
      exit({invalid_state_value, NewValue})
  end;
next ({ok, NewValue, Timeout}, State = #state{value = V, vfun = VFun, vsn = Vsn}) ->
  case eval(validation, self(), VFun, NewValue, Vsn) of
    true ->
      erlang:send_after(Timeout, self(), timeout),
      notify_watchers(V, NewValue),
      State#state{value = NewValue, vsn = erlang:make_ref()};
    false ->
      exit({invalid_state_value, NewValue})
  end;
next (Other, _State) ->
  exit({invalid_state_shift_value, Other}).

%% @hidden
%% @doc The short-run "send-off" process body.
%%
-spec send_off (pid(), agent:value_vsn(), any(), agent:agent_fun()) -> any().

send_off (Self, Vsn, Value, Fun) ->
  Next = eval(send_off, Self, Fun, Value, Vsn),
  Self ! {'$next', Next, {Vsn, Fun}}.

%% @hidden

spawn_send_off (Vsn, Value, Fun) ->
  Self = self(),
  erlang:spawn(?MODULE, send_off, [Self, Vsn, Value, Fun]).

%% @hidden
%% @doc Notfies the watching processes that the agent's state value has been set.
%% Not necessairly changed.
%%
-spec notify_watchers (any(), any()) -> any().

notify_watchers (OldVal, NewVal) ->
  {monitors, Monitors} = erlang:process_info(self(), monitors),
  lists:foreach(fun ({process, Watcher}) ->
                    Watcher ! {'agent-value', self(), OldVal, NewVal}
                end, Monitors).
