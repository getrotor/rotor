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
    {ok, {SupFlags, ChildSpecs}}.
