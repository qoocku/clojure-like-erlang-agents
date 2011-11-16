-module (agent_proc_tests).
-compile ({parse_transform, mixins_pt}).

-mixins ([agent_process_test_suit]).

-include_lib ("eunit/include/eunit.hrl").

all_test_ () ->
  all_tests(non_otp, ?MODULE).
