-module (agent_otp_tests).
-compile ({parse_transform, mixins_pt}).

-mixins ([agent_process_test_suit]).

-include_lib ("eunit/include/eunit.hrl").

all_test_ () ->
  all_tests(otp, ?MODULE).
