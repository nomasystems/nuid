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
-module(nuid).

%%% INCLUDE FILES
-include("nuid.hrl").

%%% EXTERNAL EXPORTS
%RFC 4122
-export([uuid1/0, uuid3/2, uuid4/0, uuid5/2]).
% New UUID Formats. draft-peabody-dispatch-new-uuid-format-04
-export([uuid6/0, uuid7/0]).
-export([uuid1_info/1, uuid6_info/1, uuid7_info/1]).

-export([nuid1/0, nuid1_info/1]).
-export([nuid2/0, nuid2_info/1]).

%%% MACROS
-define(JANUARY_1ST_1970, 62167219200).
-define(INTEGER_38_BIT_WRAP, 274877906944).
-define(BIT24_SPACE, 16777216).
-define(BITS24, 24).
-define(RAND_BYTES, 16).

-define(V1, 1).
% -define(V2, 2).  %%There are no v2 implementations
-define(V3, 3).
-define(V4, 4).
-define(V5, 5).
-define(V6, 6).
-define(V7, 7).
% -define(V8, 8). %%Not will be used at the moment

-define(PRECISION, 16).
-define(VARIANT, 2).

-define(HEX(X), X + $0 + (39 * (X div 10))).
-define(INT(X), ((X - $0) - ((X div $A) * 7) - ((X div $a) * 32))).

%%% RECORDS

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
uuid1() ->
    Timestamp = erlang:system_time(micro_seconds),
    Unique = erlang:unique_integer([positive, monotonic]),
    % Ensure increasing GUIDs on 38 bit wrap
    Time = Timestamp + (Unique div ?INTEGER_38_BIT_WRAP),
    <<TimeHigh:8, TimeLow:48>> = <<Time:56>>,
    <<UniqueHigh:4, ClockSeq:14, UniqueLow:20>> = <<Unique:38>>,
    NodeId = erlang:phash2(node()),
    format_uuid(
        <<TimeLow:48, ?V1:4, TimeHigh:8, UniqueHigh:4, ?VARIANT:2, ClockSeq:14, UniqueLow:20,
            NodeId:28>>,
        0,
        []
    ).

uuid1_info(Bin) ->
    uuid_info(Bin, uuid1).

uuid3(dns, Name) when is_binary(Name) ->
    compose_uuid(md5, ?V3, <<16#6ba7b8109dad11d180b400c04fd430c8:128, Name/binary>>);
uuid3(url, Name) when is_binary(Name) ->
    compose_uuid(md5, ?V3, <<16#6ba7b8119dad11d180b400c04fd430c8:128, Name/binary>>);
uuid3(oid, Name) when is_binary(Name) ->
    compose_uuid(md5, ?V3, <<16#6ba7b8129dad11d180b400c04fd430c8:128, Name/binary>>);
uuid3(x500, Name) when is_binary(Name) ->
    compose_uuid(md5, ?V3, <<16#6ba7b8149dad11d180b400c04fd430c8:128, Name/binary>>);
uuid3(nil, Name) when is_binary(Name) ->
    compose_uuid(md5, ?V3, <<0:128, Name/binary>>);
uuid3(NameSpace, Name) when is_binary(NameSpace), is_binary(Name) ->
    compose_uuid(md5, ?V3, <<NameSpace/binary, Name/binary>>).

uuid4() ->
    <<TimeLowMid:48, _Version:4, TimeHi:12, _Variant:2, ClockAndNode:62>> =
        crypto:strong_rand_bytes(16),
    format_uuid(<<TimeLowMid:48, ?V4:4, TimeHi:12, ?VARIANT:2, ClockAndNode:62>>, 0, []).

uuid5(dns, Name) when is_binary(Name) ->
    compose_uuid(sha, ?V5, <<16#6ba7b8109dad11d180b400c04fd430c8:128, Name/binary>>);
uuid5(url, Name) when is_binary(Name) ->
    compose_uuid(sha, ?V5, <<16#6ba7b8119dad11d180b400c04fd430c8:128, Name/binary>>);
uuid5(oid, Name) when is_binary(Name) ->
    compose_uuid(sha, ?V5, <<16#6ba7b8129dad11d180b400c04fd430c8:128, Name/binary>>);
uuid5(x500, Name) when is_binary(Name) ->
    compose_uuid(sha, ?V5, <<16#6ba7b8149dad11d180b400c04fd430c8:128, Name/binary>>);
uuid5(nil, Name) when is_binary(Name) ->
    compose_uuid(sha, ?V5, <<0:128, Name/binary>>);
uuid5(NameSpace, Name) when is_binary(NameSpace), is_binary(Name) ->
    compose_uuid(sha, ?V5, <<NameSpace/binary, Name/binary>>).

uuid6() ->
    Timestamp = erlang:system_time(micro_seconds),
    Unique = erlang:unique_integer([positive, monotonic]),
    % Ensure increasing GUIDs on 38 bit wrap
    Time = Timestamp + (Unique div ?INTEGER_38_BIT_WRAP),
    <<TimeHigh:48, TimeLow:8>> = <<Time:56>>,
    <<UniqueHigh:4, ClockSeq:14, UniqueLow:20>> = <<Unique:38>>,
    NodeId = erlang:phash2(node()),
    format_uuid(
        <<TimeHigh:48, ?V6:4, TimeLow:8, UniqueHigh:4, ?VARIANT:2, ClockSeq:14, UniqueLow:20,
            NodeId:28>>,
        0,
        []
    ).

uuid6_info(Bin) ->
    uuid_info(Bin, uuid6).

uuid7() ->
    Timestamp = erlang:system_time(milli_seconds),
    <<RandA:12, RandB:62, _Rest:6>> = crypto:strong_rand_bytes(10),
    format_uuid(<<Timestamp:48, ?V7:4, RandA:12, ?VARIANT:2, RandB:62>>, 0, []).

uuid7_info(Bin) ->
    uuid_info(Bin, uuid7).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL PROPOSED FUNCTIONS
%%%-----------------------------------------------------------------------------
nuid1() ->
    Timestamp = erlang:system_time(micro_seconds),
    Unique = erlang:unique_integer([positive, monotonic]),
    % Ensure increasing IDs on 38 bit wrap
    Time = Timestamp + (Unique div ?INTEGER_38_BIT_WRAP),
    Rand = crypto:strong_rand_bytes(?RAND_BYTES),
    TimeBin = to_hex(Time),
    RandBase64 = nuid_base64:encode(Rand),
    <<TimeBin/binary, "-", RandBase64/binary>>.

nuid1_info(<<HexTime:13/binary, "-", _Rand/binary>>) ->
    Time = erlang:binary_to_integer(HexTime, 16),
    calendar:gregorian_seconds_to_datetime((Time div 1000000) + ?JANUARY_1ST_1970).

nuid2() ->
    Timestamp = erlang:system_time(seconds),
    Unique = erlang:unique_integer([positive, monotonic]),
    Node = erlang:phash2(node(), ?BIT24_SPACE),
    Rand = crypto:strong_rand_bytes(?RAND_BYTES),
    nuid_base64:encode(<<Timestamp:32, Unique:32, Node:24, Rand/binary>>).

nuid2_info(Id) when is_binary(Id) ->
    <<Time:32, _Unique:32, NodeId:24, _Rand/binary>> = nuid_base64:decode(Id),
    Node = proplists:get_value(
        NodeId,
        [{erlang:phash2(X, ?BIT24_SPACE), X} || X <- [node() | nodes()]]
    ),
    {Node, calendar:gregorian_seconds_to_datetime(Time + ?JANUARY_1ST_1970)}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
compose_uuid(Cipher, Version, Binary) ->
    Hash = crypto:hash(Cipher, Binary),
    <<TimeLowMid:48, _Version:4, TimeHi:12, _Variant:2, ClockAndNode:62, _Rest/binary>> = Hash,
    format_uuid(<<TimeLowMid:48, Version:4, TimeHi:12, ?VARIANT:2, ClockAndNode:62>>, 0, []).

format_uuid(_Bin, ?PRECISION, Acc) ->
    list_to_binary(lists:reverse(Acc));
format_uuid(Bin, Pos, Acc) when Pos == 3; Pos == 5; Pos == 7; Pos == 9 ->
    Byte = binary:at(Bin, Pos),
    High = Byte div 16,
    Low = Byte rem 16,
    format_uuid(Bin, Pos + 1, [$-, ?HEX(Low), ?HEX(High) | Acc]);
format_uuid(Bin, Pos, Acc) ->
    Byte = binary:at(Bin, Pos),
    High = Byte div 16,
    Low = Byte rem 16,
    format_uuid(Bin, Pos + 1, [?HEX(Low), ?HEX(High) | Acc]).

to_hex(Id) ->
    to_hex(Id, <<>>).

to_hex(0, <<>>) ->
    <<"0">>;
to_hex(0, Acc) ->
    Acc;
to_hex(Id, Acc) ->
    Mod = Id rem 16,
    Div = Id div 16,
    to_hex(Div, <<(?HEX(Mod)), Acc/binary>>).

uuid_info(<<Data1:64, $-, Data2:32, $-, Data3:32, $-, Data41:32, $-, Data42:96>>, Type) ->
    uuid_info(<<Data1:64, Data2:32, Data3:32, Data41:32, Data42:96>>, 0, <<>>, Type).
uuid_info(_Bin, 32, Info, uuid1) ->
    get_uuid1_info(Info);
uuid_info(_Bin, 32, Info, uuid6) ->
    get_uuid6_info(Info);
uuid_info(_Bin, 32, Info, uuid7) ->
    get_uuid7_info(Info);
uuid_info(Bin, Pos, Acc, Type) ->
    H1 = binary:at(Bin, Pos),
    H2 = binary:at(Bin, Pos + 1),
    Byte = (?INT(H1) * 16) + ?INT(H2),
    uuid_info(Bin, Pos + 2, <<Acc/binary, Byte>>, Type).

get_uuid1_info(
    <<TimeLow:48, _:4, TimeHigh:8, UniqueHigh:4, _:2, ClockSeq:14, UniqueLow:20, NodeId:28>>
) ->
    <<Time:56>> = <<TimeHigh:8, TimeLow:48>>,
    <<Unique:38>> = <<UniqueHigh:4, ClockSeq:14, UniqueLow:20>>,
    get_uuid16_info(Time, Unique, NodeId).

get_uuid6_info(
    <<TimeHigh:48, _:4, TimeLow:8, UniqueHigh:4, _:2, ClockSeq:14, UniqueLow:20, NodeId:28>>
) ->
    <<Time:56>> = <<TimeHigh:48, TimeLow:8>>,
    <<Unique:38>> = <<UniqueHigh:4, ClockSeq:14, UniqueLow:20>>,
    get_uuid16_info(Time, Unique, NodeId).

get_uuid16_info(Time, Unique, NodeId) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(
        (Time div 1000000) + ?JANUARY_1ST_1970
    ),
    NodeIdF = fun(Node) ->
        Id = erlang:phash2(Node),
        <<Id28:28>> = <<Id:28>>,
        Id28
    end,
    #uuidInfo{
        date = {{Year, Month, Day}, {Hour, Min, Sec}},
        id = Unique,
        node = proplists:get_value(NodeId, [{NodeIdF(X), X} || X <- [node() | nodes()]])
    }.

get_uuid7_info(<<Time:48, _Rest/binary>>) ->
    calendar:gregorian_seconds_to_datetime((Time div 1000) + ?JANUARY_1ST_1970).
