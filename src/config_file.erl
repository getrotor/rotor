-module(config_file).
-include("common.hrl").


-export([build_config/0, build_config/1]).

%% NOTE(varoun): Config file location hard coded - will need a better way to do
%% this.
-define(CONFIG_FILE, "etc/rotor.conf").

%%%% ---------------------------------------------------------------------------

read_config(ConfigFile) ->
    {ok, Configs} = file:consult(ConfigFile),
    Configs.

build_config_record([{rotation, Rotation},
                     {check_type, http},
                     {check_url, CheckURL},
                     {algorithm, Algorithm},
                     {frequency, Frequency},
                     {timeout, Timeout},
                     {reals, Reals}]) ->
    #config_http{rotation=Rotation,
                 check_url=CheckURL,
                 algorithm=Algorithm,
                 frequency=Frequency,
                 timeout=Timeout,
                 reals=Reals}.

build_config() ->
    build_config(?CONFIG_FILE).
build_config(ConfigFile) ->
    [build_config_record(Config) || Config <- read_config(ConfigFile)].


%% [varoun@ip-10-0-2-194 rotor]$ rebar3 shell
%% ===> Verifying dependencies...
%% ===> Compiling rotor
%% Erlang/OTP 18 [erts-7.0] [source-4d83b58] [64-bit] [async-threads:0] [hipe] [kernel-poll:false]

%% Eshell V7.0  (abort with ^G)
%% 1> rr("include/records.hrl").
%% [config_http]
%% 2> config_file:build_config().
%% [#config_http{rotation = "www.freebsd.org",
%%               check_type = http,check_url = "/status.html",
%%               algorithm = round_robin,frequency = 30000,timeout = 2000,
%%               reals = ["www1.sp1.freebsd.org","www2.sp1.freebsd.org"]}]
%% 3>
