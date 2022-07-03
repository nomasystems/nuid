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

%%-----------------------------------------------------------------------------
%%% PROPERTIES
%%%-----------------------------------------------------------------------------
-type option() :: uniqueness | order.


%%% Tests two properties:
%%% 1.- uniqueness.
%%% 2.- order
-spec properties(option()) -> term().
properties(uniqueness) ->
    ?FORALL(
        Nuid,
        nuid:nuid2(),
        begin
            NewerNuid = nuid:nuid2(),
            Different = NewerNuid /= Nuid,
            Result = case Different of
                         true ->
                             true;
                         false ->
                             io:format("Expected newer nuid ~p to be different than: ~p~n",
                                       [NewerNuid, Nuid]),
                             false
                     end,
            Result == true
        end
    );
properties(order) ->
    ?FORALL(
        Nuid,
        nuid:nuid2(),
        begin
            NewerNuid = nuid:nuid2(),
            Greater = NewerNuid > Nuid,
            Result = case Greater of
                         true ->
                             true;
                         false ->
                             io:format("Expected newer nuid ~p to be greater than: ~p~n",
                                       [NewerNuid, Nuid]),
                             false
                     end,
            Result == true
        end
    ).

