%%% Copyright 2022 Nomasystems, S.L. http://www.nomasystems.com
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
%%%
-module(nuid_bench).

%%% EXTERNAL EXPORTS
-export([bench/0, profile/0]).

-define(SEPARATOR, io:format("--------------------------------------------------------------------------------------~n")).
-define(TIMES , 1000000).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
bench() ->
    Times = ?TIMES,
    Nuid1Time = erlperf:time(fun() -> nuid:nuid1() end, Times),
    Nuid2Time = erlperf:time(fun() -> nuid:nuid2() end, Times),
    Uuid1Time = erlperf:time(fun() -> nuid:uuid1() end, Times),
    Uuid4Time = erlperf:time(fun() -> nuid:uuid4() end, Times),
    Uuid6Time = erlperf:time(fun() -> nuid:uuid6() end, Times),
    Uuid7Time = erlperf:time(fun() -> nuid:uuid7() end, Times),
    AvgNuid1 = Nuid1Time/Times,
    AvgNuid2 = Nuid2Time/Times,
    AvgUuid1 = Uuid1Time/Times,
    AvgUuid4 = Uuid4Time/Times,
    AvgUuid6 = Uuid6Time/Times,
    AvgUuid7 = Uuid7Time/Times,
    io:format("nuid1 creation time: ~.2f (us)~n", [AvgNuid1]),
    io:format("nuid2 creation time: ~.2f (us)~n", [AvgNuid2]),
    io:format("uuid1 creation time: ~.2f (us)~n", [AvgUuid1]),
    io:format("uuid4 creation time: ~.2f (us)~n", [AvgUuid4]),
    io:format("uuid6 creation time: ~.2f (us)~n", [AvgUuid6]),
    io:format("uuid7 creation time: ~.2f (us)~n", [AvgUuid7]).

profile() ->
    eflambe:apply({nuid, uuid6, []}, [{output_format, brendan_gregg}]),
    eflambe:apply({nuid, nuid2, []}, [{output_format, brendan_gregg}]).
