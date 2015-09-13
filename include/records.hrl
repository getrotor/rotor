%% Various records used by rotor.

-record(config_http,
        {rotation, check_type=http, check_url, algorithm,
         frequency, timeout, reals}).
