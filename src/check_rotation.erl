-module(check_rotation).
-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/1, check/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%%%% API -----------------------------------------------------------------------

%%% For http checks.
start_link(#rconf{rotation=Rotation, ping_protocol=http} = Config) ->
    gen_server:start_link({local, list_to_atom(Rotation)},
                          ?MODULE, Config, []).

check(Rotation) ->
    gen_server:call(list_to_atom(Rotation), check_rotation).

%%%% gen_server callbacks ------------------------------------------------------

%%TODO(varoun): Should we add a bit of randomness to the frequency that
%% we use to call check_http from here ?

init(#rconf{} = Config) ->
    timer:send_after(?WAITTIME, self(), trigger),
    {ok, [Config, {status, init}]}.

handle_call(check_rotation, _From, [_Config, {status, Status}]
            = State) ->
    {reply, Status, State}.

handle_cast(_Request, Options) ->
    {noreply, Options}.

handle_info(trigger,
            [#rconf{check_interval=Interval,
                    reals=IPs,
                    ping_port=Port} = Config,
             {status, _Status}]
            = _State) ->

    Status = [[{real, IP},
               {status,
                check_http:check(list_to_atom(IP
                                              ++ ":"
                                              ++ integer_to_list(Port)))}]
              || IP <- IPs],
    timer:send_after(Interval, self(), trigger),
    {noreply, [Config, {status, Status}]};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
