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

-module(udp_server_supervisor).
-behaviour(supervisor).

-include("common.hrl").

%% API
-export([start_link/1]).

%% Callbacks
-export([init/1]).

%% Start the UDP server
start_link(#gconf{} = Config) ->
    supervisor:start_link({local, rotd_udp_server_sup}, ?MODULE, Config).

%% Callbacks
init(#gconf{} = Config) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 1},
    ChildSpecs = [#{id => rotd_udp_server_sup,
                    start => {udp_server, start_link, [Config]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [udp_server]}],
    {ok, {SupFlags, ChildSpecs}}.
