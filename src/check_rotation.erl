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

-module(check_rotation).
-behaviour(gen_server).

-include("common.hrl").

%% API
-export([start_link/1, check/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%%%% API -----------------------------------------------------------------------

%%% For all checks.
start_link(#rconf{rotation=Rotation} = Config) ->
    gen_server:start_link({local, list_to_atom(Rotation)},
                          ?MODULE, Config, []).

check(Rotation) ->
    gen_server:call(list_to_atom(Rotation), check_rotation).

%%%% gen_server callbacks ------------------------------------------------------

%%TODO(varoun): Should we add a bit of randomness to the frequency that
%% we use to call check_http from here ?

init(#rconf{} = Config) ->
    {ok, [Config, {status, init}]}.

handle_call(check_rotation, _From, [_Config, {status, Status}]
            = State) ->
    {reply, Status, State}.

handle_cast(trigger,
            [#rconf{rotation=Rotation,
                    reals=IPs,
                    ping_port=Port} = Config,
             {status, _Status}]
            = _State) ->

    Status = [[{real, IP},
               {status,
                check_real:check(list_to_atom(IP
                                              ++ ":"
                                              ++ integer_to_list(Port)))}]
              || IP <- IPs],
    gen_server:cast(list_to_atom("nameserver_" ++ Rotation), trigger),
    {noreply, [Config, {status, Status}]}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
