-module (agent_ref, [Agent]).
-export ([new/1,
          set/2,
          get/1,
          state/0,
          pid/0,
          is/1,
          send/1,
          send_off/1,
          add_watch/1]).

-type ref () :: {?MODULE, agent:agent_ref()}.
-export_types ([ref/0]).

-spec new (agent:agent_ref()) -> ref().

new (A) ->
  ?MODULE:instance(A).

-spec set (agent:property(), agent:property_value()) -> ok.

set (Property, Value) ->
  agent:set(Agent, Property, Value).

-spec get (agent:property()) -> ok.

get (Property) ->
  agent:get(Agent, Property).

-spec state () -> {ok, any()}.

state () ->
  agent:state(Agent).

-spec pid () -> {ok, pid()}.

pid () ->
  agent:pid(Agent).

-spec is (agent:agent_ref() | ref() | alive) -> boolean().

is ({?MODULE, Agent0}) ->
  agent:is(Agent, Agent0);
is (AgentOrProperty) ->
  agent:is(Agent, AgentOrProperty).

-spec send (agent:agent_fun()) -> ok.

send (Fun) ->
  agent:send(Agent, Fun).

-spec send_off (agent:agent_fun()) -> ok.

send_off (Fun) ->
  agent:send_off(Agent, Fun).

-spec add_watch (agent:watch_fun()) -> ok.

add_watch (Fun) ->
  agent:add_watch(Agent, Fun).
