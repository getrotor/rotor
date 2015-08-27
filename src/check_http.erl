-module(check_http).
-behaviour(gen_server).

%% API
-export([start_link/1, check/1]).

%% gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2,
        handle_info/2, terminate/2, code_change/3]).

%%%% API -----------------------------------------------------------------------

%% TODO(varoun) : Need to use IP addresses instead of hostnames!

start_link([{real, HostName},
            {path, _Path},
            {timeout, _Timeout},
            {frequency, _Frequency}] = Options) ->
    gen_server:start_link({local, list_to_atom(HostName)}, ?MODULE, Options, []).

check(Hostname) ->
    gen_server:call(list_to_atom(Hostname), check_health).

%%%% gen_server callbacks ------------------------------------------------------

init([{real, HostName},
      {path, Path},
      {timeout, Timeout},
      {frequency, Frequency}] = _Options) ->
    timer:send_after(Frequency, self(), trigger),
    {ok, [{url, "http://" ++ HostName ++ Path}, {timeout, Timeout},
          {frequency, Frequency}, {status, init}]}.


handle_call(check_health, _From,
            [_URL, _Timeout, _Frequency, {status, Status}] = State) ->
    {reply, Status, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(trigger, [{url, URL}, {timeout, Timeout},
                      {frequency, Frequency}, {status, _Status}] = State) ->
    try httpc:request(get, {URL, []}, [{timeout, Timeout}], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} ->
            timer:send_after(Frequency, self(), trigger),
            {noreply, [{url, URL}, {timeout, Timeout},
                       {frequency, Frequency}, {status, healthy}]};
        {error, Reason} ->
            timer:send_after(Frequency, self(), trigger),
            {noreply, [{url, URL}, {timeout, Timeout},
                       {frequency, Frequency}, {status, {unhealthy, Reason}}]}
    catch
        _:Reason ->
            {stop, Reason, State}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% 1> c("/home/varoun/dev/rotor/src/check_http", [{outdir, "/home/varoun/dev/rotor/src/"}]).
%% c("/home/varoun/dev/rotor/src/check_http", [{outdir, "/home/varoun/dev/rotor/src/"}]).
%% {ok,check_http}
%% 2> check_http:start_link([{real, "www.freebsd.org"}, {path, "/"}, {timeout, 2000},
%% 2>                        {frequency, 30000}]).
%% {ok,<0.40.0>}
%% 3> inets:start().
%% ok
%% 4> check_http:check("www.freebsd.org").
%% init
%% 5> check_http:check("www.freebsd.org").
%% healthy
%% 6> check_http:check("www.freebsd.org").
%% healthy
%% 7>
