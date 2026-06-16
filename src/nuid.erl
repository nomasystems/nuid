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
-moduledoc """
Unique identifier generation.

This module produces two families of identifiers as `t:t/0` binaries:

- **UUIDs.** Versions 1, 3, 4, and 5 per
  [RFC 4122](https://www.rfc-editor.org/rfc/rfc4122), and versions 6, 7,
  and 8 per [RFC 9562](https://www.rfc-editor.org/rfc/rfc9562), plus the
  nil and max UUIDs. Rendered as the canonical 36-character hyphenated
  form, e.g. `<<"018b3d7a-9f9a-7577-adb2-08761e3d87f7">>`.
- **nuids.** `nuid1/0` and `nuid2/0`, two Nomasystems identifiers that
  are lexicographically sortable by creation time and carry 128 bits of
  cryptographically strong randomness. They are encoded with a URL-safe,
  sortable base64 variant (see `m:nuid_base64`).

Time-based and random identifiers (`uuid1/0`, `uuid4/0`, `uuid6/0`,
`uuid7/0`, `nuid1/0`, `nuid2/0`) are non-deterministic. The name-based
identifiers (`uuid3/2`, `uuid5/2`) are deterministic: the same
`t:namespace/0` and name always produce the same UUID.

The `*_info/1` functions recover the creation time (and, where encoded,
the originating node) from an identifier.

## Examples

```erlang
1> nuid:uuid4().
<<"37a9e737-f680-44a9-b83d-a517ec758b75">>
2> nuid:uuid5(dns, <<"nomasystems.com">>).
<<"cefe05b2-95ca-5b0a-ad06-9b3f2b38e532">>
3> nuid:uuid7().
<<"018b3d7a-9f9a-7577-adb2-08761e3d87f7">>
4> nuid:nuid2().
<<"OHtpP-----Fkn3F6JaT5Kxnm_NAiDzFgGMzc">>
```
""".

%%% INCLUDE FILES
-include_lib("nuid/include/nuid.hrl").

%%% EXTERNAL EXPORTS
%RFC 4122
-export([uuid1/0, uuid3/2, uuid4/0, uuid5/2]).
% RFC 9562 (formerly draft-ietf-uuidrev-rfc4122bis)
-export([uuid6/0, uuid7/0, uuid8/1, uuid8/3]).
-export([uuid1_info/1, uuid6_info/1, uuid7_info/1]).

-export([nil_uuid/0, max_uuid/0]).

-export([nuid1/0, nuid1_info/1]).
-export([nuid2/0, nuid2_info/1]).

%%% TYPES
-export_type([t/0, namespace/0, uuid_info/0, datetime/0]).

-doc "A generated identifier as a printable binary.".
-type t() :: binary().

-doc """
Namespace for name-based UUIDs. The atoms select the predefined RFC 4122
namespace UUIDs; a binary is used verbatim as a custom namespace.
""".
-type namespace() :: dns | url | oid | x500 | nil | binary().

-doc """
Decoded creation time, embedded counter, and originating node of a
time-based UUID. Defined by the `uuidInfo` record in `nuid.hrl`.
""".
-type uuid_info() :: #uuidInfo{}.

%%% MACROS
-define(JANUARY_1ST_1970, 62167219200).
-define(INTEGER_38_BIT_WRAP, 274877906944).
-define(BIT24_SPACE, 16777216).
-define(RAND_BYTES, 16).

-define(V1, 1).
% -define(V2, 2).  %%There are no v2 implementations
-define(V3, 3).
-define(V4, 4).
-define(V5, 5).
-define(V6, 6).
-define(V7, 7).
-define(V8, 8).

-define(PRECISION, 16).
-define(VARIANT, 2).

-define(HEX(X), X + $0 + (39 * (X div 10))).
-define(INT(X), ((X - $0) - ((X div $A) * 7) - ((X div $a) * 32))).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-doc "Generate a time-based RFC 4122 version 1 UUID.".
-spec uuid1() -> t().
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

-doc "Recover creation time, counter, and node from a version 1 UUID.".
-spec uuid1_info(t()) -> uuid_info().
uuid1_info(Bin) ->
    uuid_info(Bin, uuid1).

-doc """
Generate a name-based RFC 4122 version 3 UUID (MD5).

Deterministic: the same `t:namespace/0` and name always yield the same
UUID.
""".
-spec uuid3(namespace(), binary()) -> t().
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

-doc "Generate a random RFC 4122 version 4 UUID (122 random bits).".
-spec uuid4() -> t().
uuid4() ->
    <<TimeLowMid:48, _Version:4, TimeHi:12, _Variant:2, ClockAndNode:62>> =
        crypto:strong_rand_bytes(16),
    format_uuid(<<TimeLowMid:48, ?V4:4, TimeHi:12, ?VARIANT:2, ClockAndNode:62>>, 0, []).

-doc """
Generate a name-based RFC 4122 version 5 UUID (SHA-1).

Deterministic: the same `t:namespace/0` and name always yield the same
UUID.
""".
-spec uuid5(namespace(), binary()) -> t().
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

-doc """
Generate a time-ordered RFC 9562 version 6 UUID.

The most significant bits hold the timestamp, so version 6 UUIDs sort
lexicographically by creation time.
""".
-spec uuid6() -> t().
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

-doc "Recover creation time, counter, and node from a version 6 UUID.".
-spec uuid6_info(t()) -> uuid_info().
uuid6_info(Bin) ->
    uuid_info(Bin, uuid6).

-doc """
Generate a time-ordered RFC 9562 version 7 UUID.

A 48-bit Unix millisecond timestamp followed by 74 random bits. Sorts
lexicographically by creation time.
""".
-spec uuid7() -> t().
uuid7() ->
    Timestamp = erlang:system_time(milli_seconds),
    <<RandA:12, RandB:62, _Rest:6>> = crypto:strong_rand_bytes(10),
    format_uuid(<<Timestamp:48, ?V7:4, RandA:12, ?VARIANT:2, RandB:62>>, 0, []).

-doc "Recover the creation time from a version 7 UUID.".
-spec uuid7_info(t()) -> datetime().
uuid7_info(Bin) ->
    uuid_info(Bin, uuid7).

-doc """
Generate a vendor-specific RFC 9562 version 8 UUID from a 128-bit binary.

The version and variant bits in the input are overwritten; all other
bits are preserved.
""".
-spec uuid8(binary()) -> t().
uuid8(<<CustomA:48, _Ver:4, CustomB:12, _Var:2, CustomC:62>>) ->
    uuid8(CustomA, CustomB, CustomC).

-doc """
Generate a vendor-specific RFC 9562 version 8 UUID from its three custom
fields (48, 12, and 62 bits).
""".
-spec uuid8(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> t().
uuid8(CustomA, CustomB, CustomC) ->
    format_uuid(<<CustomA:48, ?V8:4, CustomB:12, ?VARIANT:2, CustomC:62>>, 0, []).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL NIL and ZERO FUNCTIONS
%%%-----------------------------------------------------------------------------
-doc "Return the RFC 4122 nil UUID (all zero bits).".
-spec nil_uuid() -> t().
nil_uuid() ->
    <<"00000000-0000-0000-0000-000000000000">>.

-doc "Return the RFC 9562 max UUID (all one bits).".
-spec max_uuid() -> t().
max_uuid() ->
    <<"FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF">>.

%%%-----------------------------------------------------------------------------
%%% EXTERNAL PROPOSED FUNCTIONS
%%%-----------------------------------------------------------------------------
-doc """
Generate a `nuid1` identifier.

A hex microsecond timestamp, a separator, and 16 cryptographically
strong random bytes in sortable base64. Lexicographically sortable and
greater than any previously generated version 6 UUID.
""".
-spec nuid1() -> t().
nuid1() ->
    Timestamp = erlang:system_time(micro_seconds),
    Unique = erlang:unique_integer([positive, monotonic]),
    % Ensure increasing IDs on 38 bit wrap
    Time = Timestamp + (Unique div ?INTEGER_38_BIT_WRAP),
    Rand = crypto:strong_rand_bytes(?RAND_BYTES),
    TimeBin = to_hex(Time),
    RandBase64 = nuid_base64:encode(Rand),
    <<TimeBin/binary, "-", RandBase64/binary>>.

-doc "Recover the creation time from a `nuid1` identifier.".
-spec nuid1_info(t()) -> datetime().
nuid1_info(<<HexTime:13/binary, "-", _Rand/binary>>) ->
    RawTime = erlang:binary_to_integer(HexTime, 16),
    case RawTime of
        Time when Time > 0 ->
            calendar:gregorian_seconds_to_datetime((Time div 1000000) + ?JANUARY_1ST_1970);
        _Error ->
            erlang:throw({error, badarg})
    end.

-doc """
Generate a `nuid2` identifier.

A POSIX-second timestamp, a sortable counter, 3 bytes of node origin, and
16 cryptographically strong random bytes, all in sortable base64.
Lexicographically sortable, URL-safe, and no longer than a UUID.
""".
-spec nuid2() -> t().
nuid2() ->
    Timestamp = erlang:system_time(seconds),
    Unique = erlang:unique_integer([positive, monotonic]),
    Node = erlang:phash2(node(), ?BIT24_SPACE),
    Rand = crypto:strong_rand_bytes(?RAND_BYTES),
    nuid_base64:encode(<<Timestamp:32, Unique:32, Node:24, Rand/binary>>).

-doc """
Recover the originating node and creation time from a `nuid2` identifier.

The node is resolved against the currently connected nodes; it is
`undefined` if the originating node is not reachable.
""".
-spec nuid2_info(t()) -> {node() | undefined, datetime()}.
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
