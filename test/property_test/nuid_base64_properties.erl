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
-module(nuid_base64_properties).

%%% INCLUDE FILES
-include_lib("triq/include/triq.hrl").

%%% EXPORTS
-compile([export_all, nowarn_export_all]).

%%-----------------------------------------------------------------------------
%%% PROPERTIES
%%%-----------------------------------------------------------------------------
-type option() :: symmetry | sortability | url_safety.


%%% Tests three properties:
%%% 1.- symmetry.
%%% 2.- sortability
%%% 3.- url_safety
-spec properties(option()) -> term().
properties(symmetry) ->
    ?FORALL(
        Binary,
        binary(),
        ?IMPLIES(Binary /= <<>>,
                 begin
                     Encoded = nuid_base64:encode(Binary),
                     Decoded = nuid_base64:decode(Encoded),
                     Equal = Binary == Decoded,
                     Result = case Equal of
                                  true ->
                                      true;
                                  false ->
                                      io:format("Expected original binary ~p~nto be equal than: ~p~n",
                                                [Binary, Decoded]),
                                      false
                              end,
                     Result == true
                 end
                )
      );
properties(sortability) ->
    ?FORALL(
        {Bin1, Bin2},
        {binary(), binary()},
        ?IMPLIES({Bin1, Bin2} /= {<<>>, <<>>},
                 begin
                     Encoded1 = nuid_base64:encode(Bin1),
                     Encoded2 = nuid_base64:encode(Bin2),
                     GreaterThan = Bin1 > Bin2,
                     EncodedGreaterThan = Encoded1 > Encoded2,
                     Equal = GreaterThan == EncodedGreaterThan,
                     Result = case Equal of
                                  true ->
                                      true;
                                  false ->
                                      io:format("Expected encoding of ~p~nto be greater than: ~p~n",
                                                [Bin1, Bin2]),
                                      false
                              end,
                     Result == true
                 end
                )
      );
properties(url_safety) ->
    ?FORALL(
        Binary,
        binary(),
        ?IMPLIES(Binary /= <<>>,
                 begin
                     Encoded = nuid_base64:encode(Binary),
                     Safe = uri_string:quote(Encoded),
                     Equal = Safe == Encoded,
                     Result = case Equal of
                                  true ->
                                      true;
                                  false ->
                                      io:format("Encoeded binary is not url safe: ~p~n",
                                                [Binary]),
                                      false
                              end,
                     Result == true
                 end
                )
      ).

