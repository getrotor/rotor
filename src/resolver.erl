%%
%% %CopyrightBegin%
%%
%% Copyright (c) 2015-2016, Varoun. P <contact@varoun.com>.
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

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

start_link(#rconf{rotation=Rotation} = Config) ->
    gen_server:start_link({local, list_to_atom("nameserver_" ++ Rotation)},
                          ?MODULE, Config, []).

%% TODO(varoun): start using list_to_existing_atom.
gethostbyname(Name) ->
    gen_server:call(list_to_atom("nameserver_" ++ Name), gethostbyname).

%% gen_server callbacks
init(#rconf{} = Config) ->
    %% TODO(varoun): Should we trap exits?
    %% process_flag(trap_exit, true),
    {ok, [Config, {pool,[]}]}.

%% NOTE(varoun): we do simple round robin
handle_call(gethostbyname, _From, [_Config, {pool, []}] = State) ->
    {reply, ns_tryagain, State};
handle_call(gethostbyname, _From,
            [Config, {pool, [Head|Tail]}] = _State) ->
    {reply, {ns_success, Head}, [Config, {pool, Tail ++ [Head]}]}.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(trigger,
            [#rconf{rotation=Rotation} = Config,
             {pool, Pool}]) ->
    Healthy = [Host || [{real, Host}, {status, healthy}]
                           <- check_rotation:check(Rotation)],
    NewPool = health_merge(Pool, Healthy),
    {noreply, [Config, {pool, NewPool}]}.

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
