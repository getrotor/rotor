-module(resolver).
-behaviour(gen_server).

%% API
-export([start_link/1, gethostbyname/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%% For debugging only - internal functions.
-export([health_merge/2]).

%%%% API -----------------------------------------------------------------------

start_link([{rotation, Rotation},
            _Checktype,
            _CheckURL, % Need a beter name since we do tcp checks as well.
            _Frequency,
            _Timeout,
            _Reals] = Config) ->
    gen_server:start_link({local, list_to_atom("NameServer" ++ Rotation)},
                          ?MODULE, Config, []).

gethostbyname(Name) ->
    gen_server:call(list_to_atom("NameServer" ++ Name), gethostbyname).

%% gen_server callbacks
init([{rotation, Rotation}, _Checktype, _CheckURL,
      {frequency, Frequency}, _Timeout, _Reals] = Config) ->
    check_rotation:start_link(Config),
    timer:send_after(Frequency, self(), trigger),
    {ok, [{rotation, Rotation}, {pool,[]}]}.

%% NOTE(varoun): we do simple round robin
handle_call(gethostbyname, _From, [_Rotation, {pool, []}] = State) ->
    {reply, ns_tryagain, State};
handle_call(gethostbyname, _From,
            [{rotation, Rotation}, {pool, [Head|Tail]}] = _State) ->
    {reply, {ns_success, Head}, [{rotation, Rotation}, {pool, Tail ++ [Head]}]}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(trigger, [{rotation, Rotation}, {pool, Pool}]) ->
    Healthy = [Host || [{real, Host}, {status, healthy}]
                           <- check_rotation:check(Rotation)],
    NewPool = health_merge(Pool, Healthy),
    {noreply, [{rotation, Rotation}, {pool, NewPool}]}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal functions
health_merge(Pool, Healthy) ->
    UnhealthyRemoved =
        lists:filter(fun(Host) -> lists:member(Host, Healthy) end,
                     Pool),
    HealthyToAdd = lists:subtract(Healthy, Pool),
    HealthyToAdd ++ UnhealthyRemoved. % we add new server to the front!

%% 17> resolver:health_merge([2,1,3], [1,2,3]).
%% [2,1,3]
%% 18> resolver:health_merge([2,1,3], [1,2,3,4]).
%% [4,2,1,3]
%% 19> resolver:health_merge([2,1,3], [1,2]).
%% [2,1]
%% 20> resolver:health_merge([2,1,3], [1,2,4]).
%% [4,2,1]
%% 21> resolver:health_merge([2,1,3], []).
%% []
%% 22> resolver:health_merge([], [1,2,3,4]).
%% [1,2,3,4]
%% 23>
