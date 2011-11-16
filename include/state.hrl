-ifndef (AGENTS_STATE_HRL).
-define (AGENTS_STATE_HRL, true).

-record (state, {value :: any(),
                 vfun  :: agent:validation_fun(),
                 tfun  :: agent:timeout_fun()}).

-endif.
