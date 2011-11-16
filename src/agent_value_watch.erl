%%%
%%% @doc The agent watcher (observator) process implementation.
%%% @author Damian T. Dobroczy\\'nski
%%% @since 2001-11-15
%%%
-module (agent_value_watch).
-author ("Damian T. Dobroczy\\\\'nski <qoocku@gmail.com>").
-export ([new/2,
          init/2,
          loop/2]).

%%
%% @doc Creates watcher process.
%%
-spec new (agent:agent_ref(), agent:watch_fun()) -> pid().

new (Agent, Fun) ->
  erlang:spawn(?MODULE, init, [Agent, Fun]).

%% @private
%% @doc Initialises the watcher process.
%% Before jumping to the main loop it starts monitoring the agent process.
%%
-spec init (agent:agent_ref(), agent:watch_fun()) -> none().

init (Agent, Fun) ->
  erlang:monitor(process, agent:pid(Agent)),
  ?MODULE:loop(Agent, Fun).

%% @private
%% @doc The watcher process main message loop.
%% It's capable of receiving two messages: <code>{'DOWN', _, process, AgentPid, _}</code>
%% and <code>{'agent-value', agent:agent_ref(), OldValue, NewValue}</code>.
%%
-spec loop (agent:agent_ref(), agent:watch_fun()) -> none().

loop (Agent, Fun) ->
  receive
    {'DOWN', _, process, Pid, _} ->
      case agent:is(Agent, Pid) of
        true -> 
          error_logger:info_report([{?MODULE, agent_down}, 
                                     {agent, Agent},
                                     {self, self()}]),
          ok; %% end the loop - quit.
        false ->
          error_logger:error_report([{?MODULE, unknown_agent_down}, 
                                     {downed_agent_pid, Pid}, 
                                     {agent, Agent},
                                     {self, self()}]),
          ?MODULE:loop(Agent, Fun)
      end;
    {'agent-value', Agent, OldVal, NewVal} ->
      try Fun(Agent, OldVal, NewVal) of
          _ -> ?MODULE:loop(Agent, Fun)
      catch
        E:R ->
          error_logger:error_report([{?MODULE, watch_function_failed},
                                     {reason, {E,R}},
                                     {agent, Agent},
                                     {self, self()}]),
          error
      end
  end.


