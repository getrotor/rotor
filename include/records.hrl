%% Various types and records used by rotor.

-type check() :: http | https | tcp.
-type algo() :: round_robin | weighted_round_robin | load_fb.
-type ip() :: string().
-type iplist() :: [ip()].

-record(config_http, {rotation :: string(),
                      check_type = http :: check(),
                      check_url :: string(),
                      algorithm :: algo(),
                      frequency :: integer(),
                      timeout :: integer(),
                      reals :: iplist()}).
