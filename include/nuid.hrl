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
-ifndef(nuid).
-define(nuid, true).

%%% TYPES
-type day() :: 1..31.
-type month() :: 1..12.
-type hour() :: 0..23.
-type mins() :: 0..59.
-type secs() :: 0..59.

-type date() :: {pos_integer(), month(), day()}.
-type datetime() :: {date(), time()}.
-type ip() :: {0..255, 0..255, 0..255, 0..255}.
-type id() :: non_neg_integer().
-type time() :: {hour(), mins(), secs()}.
-type guid() :: binary().

%%% RECORDS
-record(uuidInfo, {
    date :: datetime(),
    id :: integer(),
    node :: atom()
}).

% -ifndef(nuid)
-endif.
