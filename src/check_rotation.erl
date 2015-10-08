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
    _Pollers =
        [check_http:start_link(
           #realconf{ip=IP,
                     ping_protocol=Config#rconf.ping_protocol,
                     ping_port=Config#rconf.ping_port,
                     ping_path=Config#rconf.ping_path,
                     response_timeout=Config#rconf.response_timeout,
                     check_interval=Config#rconf.check_interval,
                     unhealthy_threshold=Config#rconf.unhealthy_threshold,
                     healthy_threshold=Config#rconf.healthy_threshold})
         || IP <- Config#rconf.reals],
    timer:send_after(Config#rconf.check_interval, self(), trigger),
    {ok, [Config, {status, init}]}.

handle_call(check_rotation, _From, [_Config, {status, Status}]
            = State) ->
    {reply, Status, State}.

handle_cast(_Request, Options) ->
    {noreply, Options}.

handle_info(trigger,
            [#rconf{check_interval=Interval, reals = Reals} = Config,
             {status, _Status}]
            = _State) ->
    Status = [[{real, Real}, {status, check_http:check(Real)}] || Real <- Reals],
    timer:send_after(Interval, self(), trigger),
    {noreply, [Config, {status, Status}]};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
