-module(check_rotation).
-behaviour(gen_server).

%% API
-export([start_link/1, check/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%%%% API -----------------------------------------------------------------------

%%% For http checks.
start_link([{rotation, Rotation},
            {check_type, http},
            {check_url, _CheckUrl},
            {frequency, _Frequency},
            {timeout, _TimeOut},
            {reals, _Reals}] = Options) ->
    gen_server:start_link({local, list_to_atom(Rotation)},
                          ?MODULE, Options, []).

check(Rotation) ->
    gen_server:call(list_to_atom(Rotation), check_rotation).

%%%% gen_server callbacks ------------------------------------------------------

init([{rotation, _Rotation},
      {check_type, http},
      {check_url, CheckUrl},
      {frequency, Frequency},
      {timeout, Timeout},
      {reals, Reals}] = _Options) ->
    _Pollers =
        [check_http:start_link([{real, Real},
                                {path, CheckUrl},
                                {timeout, Timeout}])
         || Real <- Reals],
    {ok, [{reals, Reals}, {frequency, Frequency}]}.

handle_call(check_rotation, _From, [{reals, Reals}, {frequency, _Frequency}]
            = Options) ->
    Status = [[{real,Real}, {status, check_http:check(Real)}] || Real <- Reals],
    {reply, Status, Options}.

handle_cast(_Request, Options) ->
    {noreply, Options}.

handle_info(_Msg, Options) ->
    {noreply, Options}.

terminate(_Reason, _Options) ->
    ok.

code_change(_OldVsn, Options, _Extra) ->
    {ok, Options}.
