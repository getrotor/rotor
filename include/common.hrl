%% Various types and records used by rotor.

-type check() :: http | https | tcp.
-type algo() :: round_robin | weighted_round_robin | load_fb.
-type ip() :: string().
-type iplist() :: [ip()].

%% TODO(varoun): Make the following type specification work, and use it in
%% parsing config files.

%% -type http_check_config() :: [{rotation, Name :: string()},
%%                               {check_type, http},
%%                               {check_url, URL :: string()},
%%                               {algorithm, ALGO :: algo()},
%%                               {frequency, Freq :: integer()},
%%                               {timeout, Timeout :: integer()},
%%                               {reals, Reals :: iplist()}].

%% -type check_config() :: http_check_config().

-record(config_http, {rotation :: string(),
                      check_type = http :: check(),
                      check_url :: string(),
                      algorithm :: algo(),
                      frequency :: integer(),
                      timeout :: integer(),
                      reals :: iplist()}).
-type config_http() :: #config_http{}.

-record(config_server, {listen :: string(),
                        port :: integer()}).
-type config_server() :: #config_server{}.

-type config_record() :: config_http() | config_server().
