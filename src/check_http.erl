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
            {timeout, _Timeout}] = Options) ->
    gen_server:start_link({local, list_to_atom(HostName)}, ?MODULE, Options, []).

check(Hostname) ->
    gen_server:call(list_to_atom(Hostname), check_health).

%%%% gen_server callbacks ------------------------------------------------------

init([{real, HostName}, {path, Path}, {timeout, Timeout}] = _Options) ->
    {ok, [{url, "http://" ++ HostName ++ Path}, {timeout, Timeout}]}.


handle_call(check_health, _From, [{url, URL}, {timeout, Timeout}] = Options) ->
    try httpc:request(get, {URL, []}, [{timeout, Timeout}], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, _Body}} ->
            {reply, healthy, Options};
        {error, Reason} ->
            {reply, {unhealthy, Reason}, Options}
    catch
        _:Reason ->
            {stop, Reason, Options}
    end.

handle_cast(_Request, Options) ->
    {noreply, Options}.

handle_info(_Msg, Options) ->
    {noreply, Options}.

terminate(_Reason, _Options) ->
    ok.

code_change(_OldVsn, Options, _Extra) ->
    {ok, Options}.
