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
start_link(#config_http{rotation=Rotation, check_type=http} = Config) ->
    gen_server:start_link({local, list_to_atom(Rotation)},
                          ?MODULE, Config, []).

check(Rotation) ->
    gen_server:call(list_to_atom(Rotation), check_rotation).

%%%% gen_server callbacks ------------------------------------------------------

%%TODO(varoun): Should we add a bit of randomness to the frequency that
%% we use to call check_http from here ?

init(#config_http{check_type=http, check_url=CheckUrl,
                  frequency=Frequency, timeout=Timeout, reals=Reals}) ->
    _Pollers =
        [check_http:start_link([{real, Real},
                                {path, CheckUrl},
                                {timeout, Timeout},
                                {frequency, Frequency}])
         || Real <- Reals],
    timer:send_after(Frequency, self(), trigger),
    {ok, [{reals, Reals}, {frequency, Frequency}, {status, init}]}.

handle_call(check_rotation, _From, [_Reals, _Frequency, {status, Status}]
            = State) ->
    {reply, Status, State}.

handle_cast(_Request, Options) ->
    {noreply, Options}.

handle_info(trigger,
            [{reals, Reals}, {frequency, Frequency}, {status, _Status}]
            = _State) ->
    Status = [[{real, Real}, {status, check_http:check(Real)}] || Real <- Reals],
    timer:send_after(Frequency, self(), trigger),
    {noreply, [{reals, Reals}, {frequency, Frequency}, {status, Status}]};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
