-module (agent_process_test_suit).

-compile (export_all).
-include_lib ("eunit/include/eunit.hrl").

-define (VFUN, fun (_) -> true end).
-define (TFUN, fun (_, V) -> {ok, V} end).
     
default_vfun () ->
  ?VFUN.
              
default_tfun () ->
  ?TFUN.

setup_suit (non_otp, Linked) ->
  erlang:process_flag(trap_exit, Linked),
  {ok, Pid} = agent:start(0, default_vfun(), default_tfun()),
  Pid;
setup_suit (otp, Linked) ->
  {ok, Agent} = agent:start_link(0, default_vfun(), default_tfun()),
  erlang:process_flag(trap_exit, Linked),
  Agent.

cleanup_suit (Agent) ->
  agent:stop(Agent).

all_tests (Type, Mod) ->
  {foreach,
   fun () -> Mod:setup_suit(Type, true) end,
   fun (Ctx) -> Mod:cleanup_suit(Ctx) end,
   [fun (Ctx) -> 
        {timeout, 12000, {atom_to_list(Fun), fun () -> Mod:Fun(Ctx) end}} 
    end || Fun <- [
                   test_start,
                   test_sync_stop,
                   test_getting_state,
                   test_getting_validation,
                   test_getting_timeout,
                   test_setting_validation,
                   test_setting_timeout,
                   test_successful_validation,
                   test_unsuccessful_validation,
                   test_timeout,
                   test_send_off,
                   test_send,
                   test_watch,
                   test_agent_ref
                  ]]}.


test_start ({gen, Agent}) ->
  ?assert(is_pid(Agent));
test_start (Agent) ->
  ?assert(is_pid(Agent)).

test_sync_stop (Agent) ->
  erlang:process_flag(trap_exit, true),
  Pid = case Agent of {gen, A} -> A; A -> A end,
  erlang:link(Pid),
  ?assertEqual(ok, agent:sync_stop(Agent)),
  ?assert(receive
            {'EXIT', Pid, normal} -> true
          after
            4000 -> false
          end).

test_getting_state (Agent, SV) ->
  test_start(Agent),
  ?assertMatch({ok, SV}, agent:state(Agent)).

test_getting_validation (Agent, DefVFun) ->
  test_start(Agent),
  ?assertMatch({ok, DefVFun}, agent:get(Agent, validation)).

test_setting_validation (Agent) ->
  test_start(Agent),
  Fun = fun (_, V) -> V =/= undefined end,
  agent:set(Agent, validation, Fun),
  test_getting_validation(Agent, Fun).

test_getting_timeout (Agent) ->
  test_getting_timeout(Agent, default_tfun()).

test_getting_validation (Agent) ->
  test_getting_validation(Agent, default_vfun()).

test_getting_state (Agent) ->
  test_getting_state(Agent, 0).

test_successful_validation (Agent) ->
  erlang:process_flag(trap_exit, true),
  Pid = case Agent of {gen, A} -> A; A -> A end,
  erlang:link(Pid),
  ?debugMsg("This test will take about 4 secs...\n"),
  VFun = fun (V) -> V >= 0 end,
  test_start(Agent),
  Test = fun (_, V) -> {ok, V+1} end,
  agent:set(Agent, validation, VFun),
  agent:send(Agent, Test),
  ?assert(receive
            {'EXIT', Pid, {invalid_state_value, _}} -> false
          after
            4000 -> true
          end).
  
test_unsuccessful_validation (Agent) ->
  erlang:process_flag(trap_exit, true),
  Pid = case Agent of {gen, A} -> A; A -> A end,
  erlang:link(Pid),
  VFun = fun (V) -> V >= 0 end,
  test_start(Agent),
  Test = fun (_, V) -> {ok, V-1} end,
  agent:set(Agent, validation, VFun),
  agent:send(Agent, Test),
  ?assert(receive
              {'EXIT', Pid, {invalid_state_value, -1}} -> true
          after
            4000 -> false
          end).

test_getting_timeout (Agent, TFun) ->
  test_start(Agent),
  ?assertMatch({ok, TFun}, agent:get(Agent, timeout)).

test_setting_timeout (Agent) ->
  test_start(Agent),  
  Fun = fun (_, V) -> {ok, V} end,
  agent:set(Agent, timeout, Fun),
  test_getting_timeout(Agent, Fun).

test_timeout (Agent) ->
  Self = self(),
  Fun  = fun (_, V) -> 
             Self ! ok, 
             {ok, V, 250} 
         end,
  test_start(Agent),
  agent:set(Agent, timeout, Fun),
  %% launch the timeout machinery
  agent:send(Agent, fun (_, V) -> {ok, V, 250} end),
  Loop = fun 
           (0, _) -> true;
           (X, L) -> receive
                       ok -> L(X-1, L)
                     after
                       4000 -> false
                     end
         end,
  %% check if the timeout machinery repeatitevly works
  ?assert(Loop(3, Loop)).

test_send (Agent) ->
  Self = self(),
  test_start(Agent),
  agent:send(Agent, fun (Him, V) -> 
                        Self ! {test, agent:pid(Him) =:= self()}, 
                        {ok, V}
                    end),
  ?assert(receive
            {test, true} -> true
          after
            4000 -> false
          end).

test_send_off (Agent) ->
  Self = self(),
  test_start(Agent),
  agent:send_off(Agent, fun (Him, V) -> 
                            Self ! {test, Him =:= self()}, 
                            {ok, V}
                        end),
  ?assert(receive
            {test, false} -> true
          after
            4000 -> false
          end).

test_watch (Agent) ->
  test_start(Agent),
  Self  = self(),
  Watch = fun (A, Old, New) ->
              Self ! {test, A =:= Agent andalso Old =/= New}
          end,
  agent:add_watch(Agent, Watch),
  Fun = fun (_, V) -> {ok, V+1} end,
  agent:send(Agent, Fun),
  ?assert(receive
            {test, true} ->
              true;
            {test, false} ->
              ?debugMsg("Something wrong with the watch function!\n"),
              true
          after
            4000 ->
              false
          end).

test_agent_ref (Agent) ->
  Pid = case Agent of {gen, A0} -> A0; A0 -> A0 end,
  Self = self(),
  Ref  = agent_ref:new(Agent),
  VF1  = fun (_) -> true end, 
  VF2  = fun (V) -> V > -1 end,
  Fun  = fun (_, V) -> {ok, V + 1, 250} end,
  Fun1 = fun (A, V) -> Self ! {send, true}, Fun(A, V) end,
  Fun2 = fun (A, V) -> Self ! {send_off, Ref:pid() =:= agent:pid(A)}, Fun(A,V) end,
  TFun = fun (_, V) -> {ok, V - 1, 250} end,
  Test = fun 
           (true, Loop) ->
             erlang:process_flag(trap_exit, true),
             erlang:link(Pid),
             Ref:send(Fun1),
             Loop(false, Loop);
           (false, Loop) ->
             AgntPid = Ref:pid(),
             receive
               {'EXIT', AgntPid, {invalid_state_value, -1}} ->
                 true;
               {send, true} ->
                 Ref:set(validation, VF2),
                 Ref:send_off(Fun2),
                 Loop(false, Loop);
               {send_off, true} ->
                 Loop(false, Loop);
               Other ->
                 ?debugFmt("Something wrong with the watch function.\n"
                           "Received ~p\n"
                           "Agent state is ~p\n", [Other, Ref:state()]),
                 Loop(false, Loop)
             after
               4000 ->
                 false
             end
         end,
  Ref:set(validation, VF1),
  Ref:set(timeout, TFun),
  ?assert(Test(true, Test)).
             
             
