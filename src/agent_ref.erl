%%%
%%% @doc Toy client agent process reference.
%%% Just because I can.
%%% @since 2011-11-16
%%% @author Damian T. Dobroczy\\'nski
%%%
-module (agent_ref, [Agent]).
-author ("Damian T. Dobroczy\\\\'nski <qoocku@gmail.com>").
-export ([new/1,
          set/2,
          get/1,
          state/0,
          pid/0,
          is/1,
          send/1,
          send_off/1,
          add_watch/1]).

-type ref () :: {?MODULE, agent:agent_ref()}. %% Type of this module instance.
-export_type ([ref/0]).

%%
%% @doc Creates a new instance of this module.
%% The parameter `Agent' is set to `A' value.
%%
-spec new (agent:agent_ref()) -> ref().

new (A) ->
  ?MODULE:instance(A).

%%
%% @doc Sets agent's property.
%% @equiv agent:set(Agent, Property, Value)
%%
-spec set (agent:property(), agent:property_value()) -> ok.

set (Property, Value) ->
  agent:set(Agent, Property, Value).

%%
%% @doc Gets agent's property.
%% @equiv agent:get(Agent, Property)
%%
-spec get (agent:property()) -> ok.

get (Property) ->
  agent:get(Agent, Property).

%%
%% @doc Returns agent's state value. 
%% @equiv agent:state(Agent)
%%
-spec state () -> {ok, any()}.

state () ->
  agent:state(Agent).

-spec pid () -> {ok, pid()}.

%%
%% @doc Returns agent's process pid.
%% @equiv agent:pid(Agent)
%%
pid () ->
  agent:pid(Agent).

%%
%% @doc Test some agent's properties or equalness.
%% @equiv agent:is(Agent, AgentOrProperty)
%%
-spec is (agent:agent_ref() | ref() | alive) -> boolean().

is (_AgentOrProperty = {?MODULE, Agent0}) ->
  agent:is(Agent, Agent0);
is (AgentOrProperty) ->
  agent:is(Agent, AgentOrProperty).

%%
%% @doc Sends a function to an agent.
%% @equiv agent:send(Agent, Fun)
%%
-spec send (agent:agent_fun()) -> ok.

send (Fun) ->
  agent:send(Agent, Fun).

%%
%% @doc Sends a function off an agent.
%% @equiv agent:send_off(Agent, Fun)
%%
-spec send_off (agent:agent_fun()) -> ok.

send_off (Fun) ->
  agent:send_off(Agent, Fun).

%%
%% @doc Adds a watching function to an agent.
%% @equiv agent:add_watch(Agent, Fun)
%%
-spec add_watch (agent:watch_fun()) -> ok.

add_watch (Fun) ->
  agent:add_watch(Agent, Fun).
