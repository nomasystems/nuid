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
-module(nuid_SUITE).

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [properties, uuid, nuid].

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

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
properties() ->
    [{userdata, [{doc, "Property testing nuid"}]}].

properties(Conf) ->
    ct_property_test:quickcheck(
        nuid_properties:properties(uniqueness),
        Conf
    ),
    ct_property_test:quickcheck(
        nuid_properties:properties(order),
        Conf
    ).

uuid() ->
    [{userdata, [{doc, "UUID API."}]}].

uuid(_Conf) ->
    %v1
    UUID1 = nuid:uuid1(),
    UUID1Info = nuid:uuid1_info(UUID1),
    ct:print("UUID1 Info: ~p", [UUID1Info]),

    %v3
    UUID3DNSKey = nuid:uuid3(dns, <<"www.nomasystems.com">>),
    UUID3DNSKey2 = nuid:uuid3(dns, <<"www.nomasystems.com">>),
    UUID3DNSKey = UUID3DNSKey2,

    UUID3URLKey = nuid:uuid3(url, <<"https://www.nomasystems.com">>),
    UUID3URLKey2 = nuid:uuid3(url, <<"https://www.nomasystems.com">>),
    UUID3URLKey = UUID3URLKey2,

    UUID3OIDKey = nuid:uuid3(oid, <<"42966.3.1">>),
    UUID3OIDKey2 = nuid:uuid3(oid, <<"42966.3.1">>),
    UUID3OIDKey = UUID3OIDKey2,

    UUID3X500Key = nuid:uuid3(x500, <<"www.nomasystems.com">>),
    UUID3X500Key2 = nuid:uuid3(x500, <<"www.nomasystems.com">>),
    UUID3X500Key = UUID3X500Key2,

    UUID3NILKey = nuid:uuid3(nil, <<"nomasystems">>),
    UUID3NILKey2 = nuid:uuid3(nil, <<"nomasystems">>),
    UUID3NILKey = UUID3NILKey2,

    UUID3CustomKey = nuid:uuid3(<<"custom">>, <<"nomasystems">>),
    UUID3CustomKey2 = nuid:uuid3(<<"custom">>, <<"nomasystems">>),
    UUID3CustomKey = UUID3CustomKey2,

    %v4
    UUID4Key = nuid:uuid4(),
    UUID4Key2 = nuid:uuid4(),
    true = UUID4Key /= UUID4Key2,

    %v5
    UUID5DNSKey = nuid:uuid5(dns, <<"www.nomasystems.com">>),
    UUID5DNSKey2 = nuid:uuid5(dns, <<"www.nomasystems.com">>),
    UUID5DNSKey = UUID5DNSKey2,

    UUID5URLKey = nuid:uuid5(url, <<"https://www.nomasystems.com">>),
    UUID5URLKey2 = nuid:uuid5(url, <<"https://www.nomasystems.com">>),
    UUID5URLKey = UUID5URLKey2,

    UUID5OIDKey = nuid:uuid5(oid, <<"42966.3.1">>),
    UUID5OIDKey2 = nuid:uuid5(oid, <<"42966.3.1">>),
    UUID5OIDKey = UUID5OIDKey2,

    UUID5X500Key = nuid:uuid5(x500, <<"www.nomasystems.com">>),
    UUID5X500Key2 = nuid:uuid5(x500, <<"www.nomasystems.com">>),
    UUID5X500Key = UUID5X500Key2,

    UUID5NILKey = nuid:uuid5(nil, <<"nomasystems">>),
    UUID5NILKey2 = nuid:uuid5(nil, <<"nomasystems">>),
    UUID5NILKey = UUID5NILKey2,

    UUID5CustomKey = nuid:uuid5(<<"custom">>, <<"nomasystems">>),
    UUID5CustomKey2 = nuid:uuid5(<<"custom">>, <<"nomasystems">>),
    UUID5CustomKey = UUID5CustomKey2,

    %v6
    UUID6 = nuid:uuid6(),
    UUID6Info = nuid:uuid6_info(UUID6),
    ct:print("UUID6 Info: ~p", [UUID6Info]),
    UUID6Key1 = nuid:uuid6(),
    UUID6Key2 = nuid:uuid6(),
    true = (UUID6Key1 < UUID6Key2),
    UUID6Info1 = nuid:uuid6_info(UUID6Key1),
    ct:print("Info: ~p", [UUID6Info1]),
    UUID6Info2 = nuid:uuid6_info(UUID6Key2),
    ct:print("Info: ~p", [UUID6Info2]),

    %v7
    UUID7Key = nuid:uuid7(),
    UUID7Info = nuid:uuid7_info(UUID7Key),
    ct:print("UUID7 Info: ~p", [UUID7Info]),

    ok.

nuid() ->
    [{userdata, [{doc, "Nomasystems UID API."}]}].

nuid(_Conf) ->
    NUID1Key = nuid:nuid1(),
    NUID1Info = nuid:nuid1_info(NUID1Key),
    ct:print("NUID1 Info: ~p", [NUID1Info]),

    NUID2Key = nuid:nuid2(),
    NUID2Info = nuid:nuid2_info(NUID2Key),
    ct:print("NUID2 Info: ~p", [NUID2Info]),
    ok.
