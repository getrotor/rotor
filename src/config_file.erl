-module(config_file).
-include("common.hrl").


-export([read_config/1,
         build_config/0, build_config/1,
         build_config_record/1]).

%% NOTE(varoun): Config file location hard coded - will need a better way to do
%% this.
-define(CONFIG_FILE, "etc/rotor.conf").

%%%% ---------------------------------------------------------------------------

-spec read_config(ConfigFile) -> Configs when
      ConfigFile :: string(),
      Configs :: [term()].

read_config(ConfigFile) ->
    {ok, [Configs]} = file:consult(ConfigFile),
    Configs.

-spec build_config_record(Config) -> ConfigRecord when
      Config :: term(),
      ConfigRecord :: config_record().

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
                 reals=Reals};
build_config_record([{listen, Listen}, {port, Port}]) ->
    #config_server{listen=Listen,
                   port=Port}.


-spec build_config() -> Result when
      Result :: [config_record()].
build_config() ->
    build_config(?CONFIG_FILE).

-spec build_config(ConfigFile) -> Result when
      ConfigFile :: string(),
      Result :: [config_record()].
build_config(ConfigFile) ->
    ConfigData = read_config(ConfigFile),
    [{server, ServerConfig}, _Rotations] = ConfigData,
    [_ServerConfig, {rotations, RotationConfigs}] = ConfigData,
    ServerConfigRecord = build_config_record(ServerConfig),
    RotationConfigRecords = [build_config_record(Rotation) ||
                                 Rotation <- RotationConfigs],
    [{serverconfig, ServerConfigRecord}, {rotationconfigs,  RotationConfigRecords}].

%% [varoun@ip-10-0-2-194 rotor]$ rebar3 shell
%% ===> Verifying dependencies...
%% ===> Compiling rotor
%% Erlang/OTP 18 [erts-7.0] [source-4d83b58] [64-bit] [async-threads:0] [hipe] [kernel-poll:false]

%% Eshell V7.0  (abort with ^G)
%% 1> rr("include/common.hrl").
%% [config_http,config_server]
%% 2> config_file:build_config().
%% [{serverconfig,#config_server{listen = "127.0.0.1",
%%                               port = 6789}},
%%  {rotationconfigs,[#config_http{rotation = "www.bsd.org",
%%                                 check_type = http,check_url = "/",algorithm = round_robin,
%%                                 frequency = 30000,timeout = 2000,
%%                                 reals = ["8.8.178.110","149.20.53.86","129.128.5.194",
%%                                          "8.8.8.8"]}]}]
%% 3>
