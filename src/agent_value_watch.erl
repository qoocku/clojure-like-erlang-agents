-module (agent_value_watch).
-export ([new/2,
          init/2,
          loop/2]).

-include_lib ("eunit/include/eunit.hrl").

new (Agent, Fun) ->
  erlang:spawn(?MODULE, init, [Agent, Fun]).

init (Agent, Fun) ->
  erlang:monitor(process, agent:pid(Agent)),
  ?MODULE:loop(Agent, Fun).

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


