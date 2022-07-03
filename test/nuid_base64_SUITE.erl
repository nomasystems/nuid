%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2017. All Rights Reserved.
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

-module(nuid_base64_SUITE).

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
all() ->
    [properties, nuid_base64_otp_6279, big, illegal].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    Config = nct_util:setup_suite(Conf),
    ct_property_test:init_per_suite(Config).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

properties() ->
    [{userdata, [{doc, "Property testing nuid_base64"}]}].

properties(Conf) ->
    ct_property_test:quickcheck(
        nuid_base64_properties:properties(symmetry),
        Conf
    ),
    ct_property_test:quickcheck(
        nuid_base64_properties:properties(sortability),
        Conf
    ).

%%-------------------------------------------------------------------------
%% OTP-5635: Some data doesn't pass through nuid_base64:decode/1 correctly.
nuid_base64_otp_5635(Config) when is_list(Config) ->
    <<"===">> = nuid_base64:decode(nuid_base64:encode("===")),
    ok.
%%-------------------------------------------------------------------------
%% OTP-6279: Make sure illegal characters are rejected when decoding.
nuid_base64_otp_6279(Config) when is_list(Config) ->
    {'EXIT', _} = (catch nuid_base64:decode("dGVzda==a")),
    ok.
%%-------------------------------------------------------------------------
%% Encode and decode big binaries.
big(Config) when is_list(Config) ->
    Big = make_big_binary(300000),
    B = nuid_base64:encode(Big),
    true = is_binary(B),
    400000 = byte_size(B),
    Big = nuid_base64:decode(B),
    ok.
%%-------------------------------------------------------------------------
%% Make sure illegal characters are rejected when decoding.
illegal(Config) when is_list(Config) ->
    %% A few samples with different error reasons. Nothing can be
    %% assumed about the reason for the crash.
    {'EXIT', _} = (catch nuid_base64:decode("()")),
    {'EXIT', _} = (catch nuid_base64:decode(<<19:8, 20:8, 21:8, 22:8>>)),
    {'EXIT', _} = (catch nuid_base64:decode(<<19, 20, 21, 22>>)),
    List = "testt list",
    {'EXIT', {{badarg, List}, _}} = (catch nuid_base64:encode(List)),
    {'EXIT', {{badarg, List}, _}} = (catch nuid_base64:decode(List)),
    ok.

%%-------------------------------------------------------------------------
make_big_binary(N) ->
    list_to_binary(mbb(N, [])).

mbb(N, Acc) when N > 256 ->
    B = list_to_binary(lists:seq(0, 255)),
    mbb(N - 256, [B | Acc]);
mbb(N, Acc) ->
    B = list_to_binary(lists:seq(0, N - 1)),
    lists:reverse(Acc, B).
