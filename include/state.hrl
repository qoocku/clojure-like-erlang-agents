-ifndef (AGENTS_STATE_HRL).
-define (AGENTS_STATE_HRL, true).

-record (state, {value :: any(),                  %% The agent's state value.
                 vfun  :: agent:validation_fun(), %% The agent's state value validation function.
                 tfun  :: agent:timeout_fun()}).  %% The agent's timeout function.

-endif.
