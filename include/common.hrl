%% Various types and records used by rotor.

%%%-----------------------------------------------------------------------------
%%% File    : common.hrl
%%% Author  : Varoun. P
%%% Purpose :
%%% Created : 04 October 2015 by Varoun. P <contact@varoun.com>
%%%-----------------------------------------------------------------------------

-author('contact@varoun.com').

%% Global Conf
-record(gconf, {
          listen = "127.0.0.1",     % IP address that the UDP server listens on.
          port = 6789,              % Port that the UDP server binds to.
          logdir = "/var/log/rotor",% Logs are written here.
          rotations = []            % A list of rotation configs.
         }).

-record(rconf, {
          rotation,                 % Name of the rotation.
          policy,                   % LB policy, ex. round_robin
          ping_protocol,            % Protocol to use - http, https, tcp.
          ping_port,                % The port to use, http = 80.
          ping_path,                % The health check url for http.
          response_timeout,
          check_interval,
          unhealthy_threshold,
          healthy_threshold,
          reals = []
         }).
