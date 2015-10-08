-module(check_http).
-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/1, check/1]).

%% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2,
        handle_info/2, terminate/2, code_change/3]).

%%%% API -----------------------------------------------------------------------

start_link(#realconf{ip=IP, ping_port=Port} = Options) ->
    gen_server:start_link({local,
                           list_to_atom(IP ++ ":" ++ integer_to_list(Port))},
                          ?MODULE,
                          Options, []).

check(Real) ->
    gen_server:call(Real, check_health).

%%%% gen_server callbacks ------------------------------------------------------

init(#realconf{ip=IP, ping_protocol=http, ping_port=Port,
               ping_path=Path, response_timeout=Timeout,
               check_interval=Interval} = _Options) ->
    timer:send_after(Interval, self(), trigger),
    {ok, [{url, "http://" ++ IP ++ ":" ++ integer_to_list(Port) ++ Path},
          {timeout, Timeout},
          {interval, Interval}, {requestid, none}, {status, init}]}.


handle_call(check_health, _From,
            [_URL, _Timeout, _Interval, _RequestID,
             {status, Status}] = State) ->
    {reply, Status, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(trigger, [{url, URL},
                      {timeout, Timeout},
                      {interval, Interval},
                      _RequestID,
                      Status] = _State) ->
    {ok, RequestID} =
        httpc:request(get, {URL, []}, [{timeout, Timeout}],
                      [{sync, false}, {receiver, self()}]),
    timer:send_after(Interval, self(), trigger),
    {noreply, [{url, URL}, {timeout, Timeout},
               {interval, Interval}, {requestid, RequestID},
               Status]};
handle_info({http, {RequestID, Result}},
            [URL, Timeout, Interval,
             {requestid, RequestID},
             _Status] = _State) ->
    case Result of
        {{_Version, 200, _ReasonPhrase}, _Headers, _Body} ->
            {noreply, [URL, Timeout, Interval,
                       {requestid, none}, {status, healthy}]};
        {{_Version, ResponseCode, _ReasonPhrase}, _Headers, _Body} ->
            {noreply, [URL, Timeout, Interval,
                       {requestid, none}, {status, {unhealthy, ResponseCode}}]};
        {error, Reason} ->
            lager:notice("Check ~p failed for reason ~p.", [URL, Reason]),
            {noreply, [URL, Timeout, Interval,
                       {requestid, none}, {status, {unhealthy, Reason}}]}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
