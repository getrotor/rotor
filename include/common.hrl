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

%% Various types and records used by rotor.

%%%-----------------------------------------------------------------------------
%%% File    : common.hrl
%%% Author  : Varoun. P
%%% Purpose :
%%% Created : 04 October 2015 by Varoun. P <contact@varoun.com>
%%%-----------------------------------------------------------------------------

-author('contact@varoun.com').

%% Common definitions.
-define(WAITTIME, 200). %% Wait time in ms.

%% Global Conf
-record(gconf, {
          listen = "127.0.0.1",     % IP address that the UDP server listens on.
          port = 6789,              % Port that the UDP server binds to.
          logdir = "/var/log/rotor",% Logs are written here.
          rotations = []            % A list of rotation configs.
         }).

%% Rotation Conf
-record(rconf, {
          rotation,                 % Name of the rotation.
          policy,                   % LB policy, ex. round_robin
          ping_protocol,            % Protocol to use - http, https, tcp.
          ping_port,                % The port to use, http = 80.
          ping_path,                % The health check url for http.
          response_timeout,         % Unhealthy if response takes longer than this.
          check_interval,           % Run a check once this many ms.
          unhealthy_threshold,      % Unhealthy if this many  consecutive checks fail.
          healthy_threshold,        % Healthy if this many consequtive checks pass.
          reals = []                % A list of IPs for real servers.
         }).

%% Real Conf
-record(realconf, {
          name,
          ip,
          ping_protocol,
          ping_port,
          ping_path,
          response_timeout,
          check_interval,
          unhealthy_threshold,
          healthy_threshold
         }).

%% Check State
-record(checkstate, {
          options,                  % The realconf record goes here.
          healthy_count = 0,
          unhealthy_count = 0,
          status = unhealthy
          }).
