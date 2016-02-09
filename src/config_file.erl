%%
%% %CopyrightBegin%
%%
%% Copyright (c) 2015-2016, Varoun. P <contact@varoun.com>.
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

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
    if
        PingProtocol =:= "tcp" ->
            PingPath = "none";
        true ->
            {ok, PingPath} = opt("ping_path", RotationOptions)
    end,
    {ok, ResponseTimeout} = opt("response_timeout", RotationOptions),
    {ok, CheckInterval} = opt("check_interval", RotationOptions),
    {ok, UnhealthyThreshold} = opt("unhealthy_threshold", RotationOptions),
    {ok, HealthyThreshold} = opt("healthy_threshold", RotationOptions),
    {ok, Reals} = opt("reals", RotationOptions),
    {ok, #rconf{rotation = Rotation,
                policy = list_to_atom(Policy),
                ping_protocol = list_to_atom(PingProtocol),
                ping_port = list_to_integer(PingPort),
                ping_path = PingPath,
                response_timeout = list_to_integer(ResponseTimeout),
                check_interval = list_to_integer(CheckInterval),
                unhealthy_threshold = list_to_integer(UnhealthyThreshold),
                healthy_threshold = list_to_integer(HealthyThreshold),
                reals = Reals}}.

make_gconf(GlobalOptions, RotationOptions) ->
    Rotations = [make_rconf(Rotation, RotationOption)
                 || [{rname, Rotation}, {roptions, RotationOption}]
                        <- RotationOptions],
    {ok, Listen} = opt("listen", GlobalOptions),
    {ok, Port} = opt("port", GlobalOptions),
    {ok, LogDir} = opt("logdir", GlobalOptions),
    {ok, #gconf{listen = Listen,
                port = list_to_integer(Port),
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
