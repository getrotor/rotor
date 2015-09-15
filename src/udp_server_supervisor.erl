-module(udp_server_supervisor).
-behaviour(supervisor).

-include("common.hrl").

%% API
-export([start_link/1]).

%% Callbacks
-export([init/1]).

%% Start the UDP server
start_link([{serverconfig, _ServerConfig},
            {rotationconfigs, _Rotations}] = Config) ->
    supervisor:start_link({local, rotd_udp_server_sup}, ?MODULE, Config).

%% Callbacks
init([{serverconfig, _ServerConfig},
      {rotationconfigs, _Rotations}] = Config) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 1},
    ChildSpecs = [#{id => rotd_udp_server_sup,
                    start => {udp_server, start_link, [Config]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [udp_server]}],
    {ok, {SupFlags, ChildSpecs}}.
