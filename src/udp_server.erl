-module(udp_server).
-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
        handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%% API -----------------------------------------------------------------------

start_link([{serverconfig, _ServerConfig},
            {rotationconfigs, _RotationConfigs}] = Config) ->
    io:format("start_link"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%%%% gen_server callbacks ------------------------------------------------------

init([{serverconfig, #config_server{listen = Listen, port = Port}},
      {rotationconfigs, _RotationConfigs}]) ->
    {ok, IP} = inet:parse_address(Listen),
    {ok, Socket} = gen_udp:open(Port, [{ip, IP},binary]),
    {ok, [{socket, Socket}, {request_count, 0}]}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({udp, Socket, Host, Port, Bin},
            [{socket, Socket}, {request_count, Count}]) ->
    Hostname = binary:bin_to_list(Bin),
    case resolver:gethostbyname(Hostname) of
        ns_tryagain ->
            gen_udp:send(Socket, Host, Port, "tryagain");
        {ns_success, IP} ->
            gen_udp:send(Socket, Host, Port, IP)
    end,
    {noreply, [{socket, Socket}, {request_count, Count + 1}]}.

terminate(_Reason, [{socket, Socket}, {request_count, _Count}]) ->
    gen_udp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
