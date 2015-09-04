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
          {frequency, Frequency}, {requestid, none}, {status, init}]}.


handle_call(check_health, _From,
            [_URL, _Timeout, _Frequency, _RequestID,
             {status, Status}] = State) ->
    {reply, Status, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(trigger, [{url, URL},
                      {timeout, Timeout},
                      {frequency, Frequency},
                      _RequestID,
                      Status] = _State) ->
    {ok, RequestID} =
        httpc:request(get, {URL, []}, [{timeout, Timeout}],
                      [{sync, false}, {receiver, self()}]),
    timer:send_after(Frequency, self(), trigger),
    {noreply, [{url, URL}, {timeout, Timeout},
               {frequency, Frequency}, {requestid, RequestID},
               Status]};
handle_info({http, {RequestID, Result}},
            [URL, Timeout, Frequency,
             {requestid, RequestID},
             _Status] = _State) ->
    case Result of
        {{_Version, 200, _ReasonPhrase}, _Headers, _Body} ->
            {noreply, [URL, Timeout, Frequency,
                       {requestid, none}, {status, healthy}]};
        {error, Reason} ->
            {noreply, [URL, Timeout, Frequency,
                       {requestid, none}, {status, {unhealthy, Reason}}]}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% varoun@ip-10-0-2-24:~/dev/rotor % rebar3 compile
%% ===> Verifying dependencies...
%% ===> Compiling rotor
%% varoun@ip-10-0-2-24:~/dev/rotor % rebar3 shell
%% ===> Verifying dependencies...
%% ===> Compiling rotor
%% Erlang/OTP 18 [erts-7.0.2] [source] [64-bit] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

%% Eshell V7.0.2  (abort with ^G)
%% 1> check_http:start_link([{real, "www.freebsd.org"}, {path, "/"}, {timeout, 2000}, {frequency, 30000}]).
%% {ok,<0.78.0>}
%% 2> check_http:check("www.freebsd.org").
%% init
%% 3> check_http:check("www.freebsd.org").
%% init
%% 4> check_http:check("www.freebsd.org").
%% healthy
%% 5> check_http:check("www.freebsd.org").
%% healthy
%% 6> q().
%% ok
%% 7> varoun@ip-10-0-2-24:~/dev/rotor % rebar3 shell
%% ===> Verifying dependencies...
%% ===> Compiling rotor
%% Erlang/OTP 18 [erts-7.0.2] [source] [64-bit] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

%% Eshell V7.0.2  (abort with ^G)
%% 1> check_http:start_link([{real, "www.gfreebsd.org"}, {path, "/"}, {timeout, 2000}, {frequency, 30000}]).
%% {ok,<0.78.0>}
%% 2>  check_http:check("www.gfreebsd.org").
%% init
%% 3>  check_http:check("www.gfreebsd.org").
%% {unhealthy,{failed_connect,[{to_address,{"www.gfreebsd.org",
%%                                          80}},
%%                             {inet,[inet],nxdomain}]}}
%% 4> q().
%% ok
%% 5> varoun@ip-10-0-2-24:~/dev/rotor %
