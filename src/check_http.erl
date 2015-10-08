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
               ping_path=Path, check_interval=Interval} = Options) ->
    timer:send_after(Interval, self(), trigger),
    {ok, [{options, Options},
          {url, "http://" ++ IP ++ ":" ++ integer_to_list(Port) ++ Path},
          {requestid, none},
          {healthy_count, 0}, {unhealthy_count, 0},
          {status, unhealthy}]}.


handle_call(check_health, _From,
            [_Options, _URL, _RequestID, _HealthyCount, _UnhealthyCount,
             {status, Status}] = State) ->
    {reply, Status, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% NOTE(varoun): Should requestid in the message be none ?
handle_info(trigger,
            [{options, Options},
             {url, URL},
             _RequestID,
             HealthyCount, UnhealthyCount,
             Status] = _State) ->
    {ok, RequestID} =
        httpc:request(get,
                      {URL, []},
                      [{timeout, Options#realconf.response_timeout}],
                      [{sync, false}, {receiver, self()}]),
    timer:send_after(Options#realconf.check_interval, self(), trigger),
    {noreply, [{options, Options},
               {url, URL},
               {requestid, RequestID},
               HealthyCount, UnhealthyCount,
               Status]};
handle_info({http, {RequestID, Result}},
            [{options, Options},
             URL,
             {requestid, RequestID},
             {healthy_count, HealthyCount},
             {unhealthy_count, UnhealthyCount},
             {status, Status}] = _State) ->
    case Result of
        {{_Version, 200, _ReasonPhrase}, _Headers, _Body}
          when Status =:= healthy
               andalso HealthyCount =:= Options#realconf.healthy_threshold
               andalso UnhealthyCount =:= 0 ->
            {noreply, [{options, Options},
                       URL,
                       {healthy_count, HealthyCount},
                       {unhealthy_count, 0},
                       {requestid, none}, {status, healthy}]};
        {{_Version, 200, _ReasonPhrase}, _Headers, _Body}
          when Status =:= unhealthy
               andalso HealthyCount < Options#realconf.healthy_threshold ->
            {noreply, [{options, Options},
                       URL,
                       {healthy_count, HealthyCount + 1},
                       {unhealthy_count, 0},
                       {requestid, none},
                       {status, unhealthy}]};
        {{_Version, 200, _ReasonPhrase}, _Headers, _Body}
          when Status =:= unhealthy
               andalso HealthyCount =:= Options#realconf.healthy_threshold ->
            {noreply, [{options, Options},
                       URL,
                       {healthy_count, HealthyCount},
                       {unhealthy_count, 0},
                       {requestid, none},
                       {status, healthy}]};
        {{_Version, _ResponseCode, _ReasonPhrase}, _Headers, _Body}
          when Status =:= unhealthy
               andalso UnhealthyCount =:= Options#realconf.unhealthy_threshold
               andalso HealthyCount =:= 0 ->
            {noreply, [{options, Options},
                       URL,
                       {healthy_count, 0},
                       {unhealthy_count, UnhealthyCount},
                       {requestid, none},
                       {status, unhealthy}]};
        {{_Version, _ResponseCode, _ReasonPhrase}, _Headers, _Body}
          when Status =:= healthy
               andalso UnhealthyCount < Options#realconf.unhealthy_threshold ->
            {noreply, [{options, Options},
                       URL,
                       {healthy_count, 0},
                       {unhealthy_count, UnhealthyCount + 1},
                       {requestid, none},
                       {status, healthy}]};
        {{_Version, _ResponseCode, _ReasonPhrase}, _Headers, _Body}
          when Status =:= healthy
               andalso UnhealthyCount =:= Options#realconf.unhealthy_threshold ->
            {noreply, [{options, Options},
                       URL,
                       {healthy_count, 0},
                       {unhealthy_count, UnhealthyCount},
                       {requestid, none},
                       {status, unhealthy}]};

        {error, _Reason}
          when Status =:= unhealthy
               andalso UnhealthyCount =:= Options#realconf.unhealthy_threshold
               andalso HealthyCount =:= 0 ->
            {noreply, [{options, Options},
                       URL,
                       {healthy_count, 0},
                       {unhealthy_count, UnhealthyCount},
                       {requestid, none},
                       {status, unhealthy}]};
        {error, _Reason}
          when Status =:= healthy
               andalso UnhealthyCount < Options#realconf.unhealthy_threshold ->
            {noreply, [{options, Options},
                       URL,
                       {healthy_count, 0},
                       {unhealthy_count, UnhealthyCount + 1},
                       {requestid, none},
                       {status, healthy}]};

        {error, _Reason}
          when Status =:= healthy
               andalso UnhealthyCount =:= Options#realconf.unhealthy_threshold ->
            {noreply, [{options, Options},
                       URL,
                       {healthy_count, 0},
                       {unhealthy_count, UnhealthyCount},
                       {requestid, none},
                       {status, unhealthy}]}
        end;

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
