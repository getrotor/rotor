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

start_link(#gconf{} = Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%%%% gen_server callbacks ------------------------------------------------------

init(#gconf{listen=Listen, port=Port, rotations=RotationConfigs}) ->
    Rotations = [Rotation#rconf.rotation || Rotation <- RotationConfigs],
    {ok, IP} = inet:parse_address(Listen),
    {ok, Socket} = gen_udp:open(Port, [{ip, IP},binary]),
    {ok, [{socket, Socket}, {request_count, 0}, {rotations, Rotations}]}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({udp, Socket, Host, Port, Bin},
            [{socket, Socket}, {request_count, Count},
             {rotations, Rotations}]) ->
    Hostname = binary:bin_to_list(Bin),

    Result = case lists:member(Hostname, Rotations) of
                 true -> resolver:gethostbyname(Hostname);
                 false -> ns_unavail
             end,
    send_response(Socket, Host, Port, Result),
    %%exometer:update([rotor, udp_server, request_count], 1),
    {noreply, [{socket, Socket}, {request_count, Count + 1},
               {rotations, Rotations}]}.




terminate(_Reason, [{socket, Socket}, _RequestCount, _Rotation]) ->
    gen_udp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions.
send_response(Socket, Host, Port, {ns_success, IP}) ->
    gen_udp:send(Socket, Host, Port, IP);
send_response(Socket, Host, Port, ns_unavail) ->
    gen_udp:send(Socket, Host, Port, "ns_unavail");
send_response(Socket, Host, Port, ns_tryagain) ->
    gen_udp:send(Socket, Host, Port, "ns_tryagain").
