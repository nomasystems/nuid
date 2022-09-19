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
-module(nuid_properties).

%%% INCLUDE FILES
-include_lib("triq/include/triq.hrl").

%%% EXPORTS
-compile([export_all, nowarn_export_all]).

%%% MACROS
-define(UUID_TYPES, [uuid1, uuid2, uuid3, uuid4, uuid5, uuid6, uuid7, nuid1, nuid2]).

%%-----------------------------------------------------------------------------
%%% PROPERTIES
%%%-----------------------------------------------------------------------------
-type option() :: uniqueness | order | uuid6_nuid1_compatibility.

%%% Tests two properties:
%%% 1.- uniqueness
%%% 2.- order
%%% 3.- uuid6 and nuid1 compatibility
-spec properties(option()) -> term().
properties(uniqueness) ->
    ?FORALL(
        UUIDType,
        triq_dom:elements(?UUID_TYPES),
        begin
            OldUUID = nuid:UUIDType(),
            NewUUID = nuid:UUIDType(),
            Different = NewUUID /= OldUUID,
            Result =
                case Different of
                    true ->
                        true;
                    false ->
                        io:format(
                            "Expected newer nuid ~p to be different than: ~p~n",
                            [NewUUID, OldUUID]
                        ),
                        false
                end,
            Result == true
        end
    );
properties(order) ->
    ?FORALL(
        UUIDType,
        triq_dom:elements(?UUID_TYPES),
        begin
            OldUUID = nuid:UUIDType(),
            NewUUID = nuid:UUIDType(),
            Greater = NewUUID > OldUUID,
            Result =
                case Greater of
                    true ->
                        true;
                    false ->
                        io:format(
                            "Expected newer nuid ~p to be greater than: ~p~n",
                            [NewUUID, OldUUID]
                        ),
                        false
                end,
            Result == true
        end
    );
properties(uuid6_nuid1_compatibility) ->
    ?FORALL(
        {UUID6, NUID1},
        {nuid:uuid6(), nuid:nuid1()},
        begin
            Greater = NUID1 > UUID6,
            Result =
                case Greater of
                    true ->
                        true;
                    false ->
                        io:format(
                            "Expected nuid1 ~p to be greater than uuid6 ~p~n",
                            [NUID1, UUID6]
                        ),
                        false
                end,
            Result == true
        end
    ).
