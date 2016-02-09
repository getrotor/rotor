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

%%%-------------------------------------------------------------------
%% @doc rotor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('rotor_sup').

-behaviour(supervisor).

-include("common.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, Config} = config_file:make_conf(),
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    UDP_server_spec = #{id => udp_server_sup_sup,
                        start => {udp_server_supervisor, start_link, [Config]},
                        restart => permanent,
                        shutdown => brutal_kill,
                        type => supervisor,
                        modules => [udp_server_supervisor]},

    RotationSpecs = [#{id => RotConfig#rconf.rotation ++ "_sup_sup",
                       start => {rotation_supervisor, start_link, [RotConfig]},
                       restart => permanent,
                       shutdown => brutal_kill,
                       type => supervisor,
                       modules => [rotation_supervisor]} ||
                        RotConfig <- Config#gconf.rotations],
    ChildSpecs = [UDP_server_spec] ++ RotationSpecs,
    {ok, {SupFlags, ChildSpecs}}.


%%====================================================================
%% Internal functions
%%====================================================================
