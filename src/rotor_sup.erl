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
    Config = config_file:build_config(),
    [{serverconfig, _ServerConfig}, {rotationconfigs, RotationConfigs}] =
        Config,
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    UDP_server_spec = #{id => udp_server_sup_sup,
                        start => {udp_server_supervisor, start_link, [Config]},
                        restart => permanent,
                        shutdown => brutal_kill,
                        type => supervisor,
                        modules => [udp_server_supervisor]},

    RotationSpecs = [#{id => RotConfig#config_http.rotation ++ "_sup_sup",
                       start => {rotation_supervisor, start_link, [RotConfig]},
                       restart => permanent,
                       shutdown => brutal_kill,
                       type => supervisor,
                       modules => [rotation_supervisor]} ||
                        RotConfig <- RotationConfigs],
    ChildSpecs = [UDP_server_spec] ++ RotationSpecs,
    {ok, {SupFlags, ChildSpecs}}.


%%====================================================================
%% Internal functions
%%====================================================================
