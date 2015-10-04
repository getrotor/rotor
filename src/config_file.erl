-module(config_file).
-include("common.hrl").


-export([make_conf/0]).

-define(CONFIG_FILE, "etc/rotor.conf").

%%%% ---------------------------------------------------------------------------

parse_config_file() ->
    {ok, Binary} = file:read_file(?CONFIG_FILE),
    {ok, Tokens, _} = config_lexer:string(binary_to_list(Binary)),
    config_parser:parse(Tokens).

make_rconf(Rotation, RotationOptions) ->
    {ok, Policy} = opt("policy", RotationOptions),
    {ok, PingProtocol} = opt("ping_protocol", RotationOptions),
    {ok, PingPort} = opt("ping_port", RotationOptions),
    {ok, PingPath} = opt("ping_path", RotationOptions),
    {ok, ResponseTimeout} = opt("response_timeout", RotationOptions),
    {ok, CheckInterval} = opt("check_interval", RotationOptions),
    {ok, UnhealthyThreshold} = opt("unhealthy_threshold", RotationOptions),
    {ok, HealthyThreshold} = opt("healthy_threshold", RotationOptions),
    {ok, Reals} = opt("reals", RotationOptions),
    {ok, #rconf{rotation = Rotation,
                policy = Policy,
                ping_protocol = PingProtocol,
                ping_port = PingPort,
                ping_path = PingPath,
                response_timeout = ResponseTimeout,
                check_interval = CheckInterval,
                unhealthy_threshold = UnhealthyThreshold,
                healthy_threshold = HealthyThreshold,
                reals = Reals}}.

make_gconf(GlobalOptions, RotationOptions) ->
    Rotations = [make_rconf(Rotation, RotationOption)
                 || [{rname, Rotation}, {roptions, RotationOption}]
                        <- RotationOptions],
    {ok, Listen} = opt("listen", GlobalOptions),
    {ok, Port} = opt("port", GlobalOptions),
    {ok, LogDir} = opt("logdir", GlobalOptions),
    {ok, #gconf{listen = Listen,
                port = Port,
                logdir = LogDir,
                rotations = [Rotation || {ok, Rotation} <- Rotations]}}.

make_conf() ->
    {ok, [GlobalOptions, RotationOptions]} = parse_config_file(),
    make_gconf(GlobalOptions, RotationOptions).




%%% ----------------------------------------------------------------------------
%%% Misc Functions.
%%% ----------------------------------------------------------------------------

opt(Option, [{Option, Value}|_]) ->
    {ok, Value};
opt(Option, [_|Options]) ->
    opt(Option, Options);
opt(_, []) ->
    false.
