-module(resolver).
-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/1, gethostbyname/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%% For debugging only - internal functions.
%% -export([health_merge/2]).

%%%% API -----------------------------------------------------------------------

start_link(#config_http{rotation=Rotation} = Config) ->
    gen_server:start_link({local, list_to_atom("nameserver_" ++ Rotation)},
                          ?MODULE, Config, []).

%% TODO(varoun): start using list_to_existing_atom.
gethostbyname(Name) ->
    gen_server:call(list_to_atom("nameserver_" ++ Name), gethostbyname).

%% gen_server callbacks
init(#config_http{rotation=Rotation, frequency=Frequency} = Config) ->
    %% TODO(varoun): Should we trap exits?
    %% process_flag(trap_exit, true),
    check_rotation:start_link(Config),
    timer:send_after(Frequency, self(), trigger),
    {ok, [{rotation, Rotation}, {frequency, Frequency}, {pool,[]}]}.

%% NOTE(varoun): we do simple round robin
handle_call(gethostbyname, _From, [_Rotation, _Freq, {pool, []}] = State) ->
    {reply, ns_tryagain, State};
handle_call(gethostbyname, _From,
            [{rotation, Rotation}, Frequency, {pool, [Head|Tail]}] = _State) ->
    {reply, {ns_success, Head}, [{rotation, Rotation},
                                 Frequency,
                                 {pool, Tail ++ [Head]}]}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(trigger, [{rotation, Rotation},
                      {frequency, Frequency},
                      {pool, Pool}]) ->
    Healthy = [Host || [{real, Host}, {status, healthy}]
                           <- check_rotation:check(Rotation)],
    NewPool = health_merge(Pool, Healthy),
    timer:send_after(Frequency, self(), trigger),
    {noreply, [{rotation, Rotation}, {frequency, Frequency}, {pool, NewPool}]}.

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
