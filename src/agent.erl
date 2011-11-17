%%%
%%% @doc Agent client API.
%%% @author Damian T. dobroczy\\'nski
%%% @since 2011-11-15
%%%
-module (agent).
-author ("Damian T. Dobroczy\\\\'nski <qoocku@gmail.com>").
-export ([% starting & stopping
          start/1,
          start/3,
          start_link/1,
          start_link/3,
          start_ref/1,
          start_ref/3,
          start_link_ref/1,
          start_link_ref/3,
          stop/1,
          sync_stop/1,
          % setting/getting features          
          set/3,
          get/2,
          pid/1,
          is/2,
          % interaction
          state/1,
          send/2,
          send_off/2,
          send_off/3,
          add_watch/2,
          % internal exports
          call/2,
          cast/2]).

%%% ===========================================================================
%%% P u b l i c  A P I 
%%% ===========================================================================

-type callback_result() :: {ok, any()} | {ok, any(), timeout()}.       %% The result of `agent_ref()' function.
-type validation_fun () :: fun ((any()) -> boolean()).                 %% The type of agent's validation function.
-type agent_fun      () :: fun ((pid(), any()) -> callback_result()).  %% The type of agent's operational function.
-type timeout_fun    () :: agent_fun().                                %% The type of agent's timeout function.
-opaque agent_ref    () :: agent_srv:ref() | pid().                    %% The type of agent's reference.
-type watch_fun      () :: fun ((agent_ref(), any(), any()) -> any()). %% The type of agent's watching function.
-type property       () :: validation | timeout | state.               %% The type of agent's property.
-type property_value () :: validation_fun() | timeout_fun() | any().   %% The type of agent's property value.
-type value_vsn      () :: reference() | undefined.                    %% the type of agent's state value version.

-export_type ([callback_result/0,
               validation_fun/0,
               agent_fun/0,
               timeout_fun/0,
               watch_fun/0,
               agent_ref/0,
               property/0,
               property_value/0,
               value_vsn/0]).

%%% Start & Stop

%%
%% @doc Starts unsupervised agent with the `InitialState' as initial value. 
%% The validation and timeout functions are set to their defaults value (which
%% results in validation accepting any state value and timeout function doing nothing).
%%
%% @see start/3
%% @see start_link/1
%%
-spec start (any()) -> {ok, agent_ref()}.

start (InitialState) ->
  do_start(start, InitialState).

%%
%% @doc Start unsupervised agent with the `InitialState' as initial value. The validation
%% function set is to `VFun' and timeout function set to `TFun'. The result contains the
%% agent reference which currently is a <em>pid</em>.
%%
%% @see start/1
%% @see start_link/3
%%
-spec start (any(), validation_fun(), timeout_fun()) -> {ok, agent_ref()}.

start (InitialState, VFun, TFun) when is_function (VFun) andalso is_function(TFun) ->
  do_start(start, [InitialState, VFun, TFun]).

%%
%% @doc Starts unsupervised agent with the `InitialState' as initial value. 
%% The validation and timeout functions are set to their defaults value (which
%% results in validation accepting any state value and timeout function doing nothing).
%% The result is parametrized module instance `agent_ref[]'.
%%
%% @see start_ref/3
%% @see start_link_ref/1
%% @see agent_ref:new/1
%%
-spec start_ref (any()) -> {ok, agent_ref:ref()}.

start_ref (InitialState) ->
  {ok, Agent} = do_start(start, InitialState),
  {ok, agent_ref:new(Agent)}.

%%
%% @doc Start unsupervised agent with the `InitialState' as initial value. The validation
%% function is set to `VFun' and timeout function is set to `TFun'. 
%% The result is parametrized module instance `agent_ref[]'.
%%
%% @see start_ref/1
%% @see start_link_ref/3
%% @see agent_ref:new/1
%%
-spec start_ref (any(), validation_fun(), timeout_fun()) -> {ok, agent_ref()}.

start_ref (InitialState, VFun, TFun) when is_function (VFun) andalso is_function(TFun) ->
  {ok, Agent} = do_start(start, [InitialState, VFun, TFun]),
  {ok, agent_ref:new(Agent)}.

%%
%% @doc Starts an agent to be supervised with initial state set to `InitialState' value.
%% The validation and timeout functions are set to their defaults value (which
%% results in validation accepting any state value and timeout function doing nothing).
%% The result is containes <em>opaque</em> agent's reference (do not confuse with `agent_ref:ref()'.
%%
%% @see start/1
%% @see start_link/3
%%
-spec start_link (any()) -> {ok, agent_ref()}.

start_link (InitialState) ->
  do_start(start_link, InitialState).

%%
%% @doc Starts an agent to be supervised with the `InitialState' as initial value, validation
%% function set to `VFun' and timeout function set to `TFun'.
%% The result contains the agent reference which currently is an <em>opaque</em> term
%% containing the agent process <em>pid</em>.
%%
%% @see start/3
%% @see start_link/1
%%
-spec start_link (any(), validation_fun(), timeout_fun()) -> {ok, agent_ref()}.

start_link (InitialState, VFun, TFun) when is_function (VFun) andalso is_function(TFun) ->
  do_start(start_link, {InitialState, VFun, TFun}).

%%
%% @doc Starts an agent to be supervised with initial state set to `InitialState' value.
%% The validation and timeout functions are set to their defaults value (which
%% results in validation accepting any state value and timeout function doing nothing).
%% The result is parametrized module instance `agent_ref[]'.
%%
%% @see start_ref/1
%% @see start_link_ref/3
%% @see agent_ref:new/1
%%
-spec start_link_ref (any()) -> {ok, agent_ref()}.

start_link_ref (InitialState) ->
  {ok, Agent} = do_start(start_link, InitialState),
  {ok, agent_ref:new(Agent)}.

%%
%% @doc Starts an agent to be supervised with the `InitialState' as initial value. The validation
%% function is set to `VFun' and timeout function is set to `TFun'.
%% The result is parametrized module instance `agent_ref[]'.
%%
%% @see start_ref/3
%% @see start_link_ref/1
%% @see agent_ref:new/1
%%
-spec start_link_ref (any(), validation_fun(), timeout_fun()) -> {ok, agent_ref()}.

start_link_ref (InitialState, VFun, TFun) when is_function (VFun) andalso is_function(TFun) ->
  {ok, Agent} = do_start(start_link, {InitialState, VFun, TFun}),
  {ok, agent_ref:new(Agent)}.

%%
%% @doc Stops an agent asynchronously.
%%
%% @see sync_stop/1
%%
-spec stop (agent_ref()) -> ok.

stop ({gen, Agent}) when is_pid(Agent) ->
  gen_server:cast(Agent, shutdown);
stop (Agent) when is_pid(Agent) ->
  Agent ! shutdown,
  ok.

%%
%% @doc Stops an agent synchronously (the caller is blocked till the agent stops).
%%
-spec sync_stop (agent_ref()) -> ok.

sync_stop ({gen, Agent}) when is_pid(Agent) ->
  gen_server:cast(Agent, shutdown);
sync_stop (Agent) when is_pid(Agent) ->
  Agent ! {self(), shutdown},
  receive
    {Agent, ok} -> ok
  after
    5000 ->
      exit(timeout)
  end.

%%% Meta-features accessors

%%
%% @doc Sets an agent property. The property may be:
%% <ul>
%% <li>`validation' -- which stands for the agent's validation function;</li>
%% <li>`timeout' -- which stands for the agent's timeout function;</li>
%% </ul>
%%
%% @see get/2
%%
-spec set (agent_ref(), property(), property_value()) -> ok.

set ({gen, Agent}, Property, Value) when is_pid(Agent) andalso
                                         (Property =:= validation orelse Property =:= timeout) andalso
                                         is_function(Value) ->
  gen_server:cast(Agent, {Property, Value});
set (Agent, Property, Value) when is_pid(Agent) andalso
                                  (Property =:= validation orelse Property =:= timeout) andalso
                                  is_function(Value) ->
  Agent ! {Property, Value},
  ok.

%%
%% @doc Returns an agent property. The property may be:
%% <ul>
%% <li>`validation' -- which stands for the agent's validation function;</li>
%% <li>`timeout' -- which stands for the agent's timeout function;</li>
%% <li>`state' -- which stands for the agent's state value.</li>
%% </ul>
%%
%% @see set/3
%% @see state/2
%%
-spec get (agent_ref(), property()) -> {ok, property_value()}.

get (Agent, Msg) ->
  ?MODULE:call(Agent, Msg).

%%
%% @doc Returns the agent process <em>pid</em>.
%%
-spec pid (agent_ref()) -> pid().

pid ({gen, Pid}) ->
  Pid;
pid (Agent) when is_pid(Agent) ->
  Agent.

%%
%% @doc Tests agent's properties. The tests includes:
%% <ul>
%% <li>the equality of agent references iff `_Arg' is an agent's reference;</li>
%% <li>the agent liveness iff `_Arg' is atom `alive'.</li>
%% </ul>
%%
-spec is (agent_ref(), agent_ref() | alive) -> boolean().

is (_Agent = {gen, Pid}, _Arg = alive) when is_pid(Pid) ->
  is(Pid, alive);
is (_Agent = {gen, Pid}, _Arg = Pid2) when is_pid(Pid) ->
  is(Pid, Pid2);
is (_Agent, alive) when is_pid(_Agent) ->
  erlang:process_info(_Agent) =/= undefined;
is (_Agent, _Arg) when is_pid(_Agent) ->
  _Agent =:= _Arg.

%%% Interaction

%%
%% @doc Returns an agent's state value.
%% @equiv agent:get(Agent, state)
%%
%% @see get/2
%%
-spec state (agent_ref()) -> {ok, any()}.
                
state (Agent) ->
  ?MODULE:get(Agent, state).

%%
%% @doc Sends to an agent a function to be evaluated asynchronously.
%% The function will be evaluated within the agent process.
%%
%% @see send_off
%%
-spec send (agent_ref(), agent_fun()) -> ok.

send (Agent, Fun) ->
  ?MODULE:cast(Agent, {send, Fun}).

%%
%% @doc Sends to an agent a function to be evaluated asynchronously in a separate process.
%% Version control.
%% @equiv send_off(Agent, Fun, true)
%% @see send/2
%% 
-spec send_off (agent_ref(), agent_fun()) -> ok.

send_off (Agent, Fun) ->
  send_off(Agent, Fun, true).

%%
%% @doc Sends to an agent a function to be evaluated asynchronously in a separate process.
%% If `Versioned' is `true' the agent's value will be versioned.
%% @equiv send_off(Agent, Fun, false)
%% @see send/2
%% 
-spec send_off (agent_ref(), agent_fun(), boolean()) -> ok.

send_off (Agent, Fun, Versioned) ->
  ?MODULE:cast(Agent, {send_off, Fun, Versioned}).

%%
%% @doc Binds to an agent a watching function. The function will be evaluated
%% when the agent's state value has been changed (i.e. <em>after</em> the validation
%% function has been evaluated and returns `true' and the new value is not the same
%% as the old one).
%%
-spec add_watch (agent_ref(), watch_fun()) -> ok.

add_watch (Agent, Fun) when is_function(Fun) ->
  ?MODULE:cast(Agent, {add_watch, Fun}).

%%% ===========================================================================
%%% P r i v a t e  S t u f f
%%% ===========================================================================

%% @private
-spec do_start (start|start_link, any() | {any(), validation_fun(), timeout_fun()}) -> {ok, agent_ref()}.

do_start (start_link, Args) ->
  {ok, Pid} = gen_server:start_link(agent_srv, Args, []),
  {ok, {gen, Pid}};
do_start (start, Args) when is_list(Args) ->
  agent_proc:start(Args);
do_start (start, Arg) ->
  agent_proc:start([Arg]).

%% @private
-spec cast (agent_ref(), agent_fun()) -> ok.

cast ({gen, Agent}, Msg) ->
  gen_server:cast(Agent, Msg);
cast (Agent, Msg) ->
  Agent ! Msg,
  ok.

%% @private
call ({gen, Agent}, Property)  when is_pid(Agent) ->
  gen_server:call(Agent, Property);
call (Agent, Property)  when is_pid(Agent) ->
  Agent ! {self(), Property},
  receive
    {Agent, Property, Value} -> {ok, Value}
  after
    5000 ->
      exit(timeout)
  end.
