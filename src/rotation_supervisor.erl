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

-module(rotation_supervisor).
-behaviour(supervisor).

-include("common.hrl").

%% API
-export([start_in_shell_for_testing/1,
         start_link/1]).

%% Callbacks
-export([init/1]).

%% Start the process needed for a single rotation

start_link(#rconf{rotation=Rotation} = Config) ->
    supervisor:start_link({local, list_to_atom(Rotation ++ "_supervisor")},
                          ?MODULE, Config).

start_in_shell_for_testing(#rconf{rotation=Rotation} = Config) ->
    {ok, Pid} = supervisor:start_link({local, list_to_atom(Rotation ++ "_supervisor")},
                                      ?MODULE, Config),
    unlink(Pid).

%% Callbacks
init(#rconf{rotation=Rotation} = Config) ->
    RealConfs = [#realconf{name=Rotation,
                           ip=IP,
                           ping_protocol=Config#rconf.ping_protocol,
                           ping_port=Config#rconf.ping_port,
                           ping_path=Config#rconf.ping_path,
                           response_timeout=Config#rconf.response_timeout,
                           check_interval=Config#rconf.check_interval,
                           unhealthy_threshold=Config#rconf.unhealthy_threshold,
                           healthy_threshold=Config#rconf.healthy_threshold}
                 || IP <- Config#rconf.reals],
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => "nameserver_" ++ Rotation,
                    start => {resolver, start_link, [Config]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [resolver]},
                  #{id => Rotation,
                    start => {check_rotation, start_link, [Config]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [check_rotation]}],
    RealSpecs = [#{id => RealConfig#realconf.ip ++
                       integer_to_list(RealConfig#realconf.ping_port) ++
                       "_sup",
                   start => {check_real, start_link, [RealConfig]},
                   restart => permanent,
                   shutdown => brutal_kill,
                   type => worker,
                   modules => [check_real]} || RealConfig <- RealConfs],
    {ok, {SupFlags, ChildSpecs ++ RealSpecs}}.
