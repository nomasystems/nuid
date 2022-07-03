%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2021. All Rights Reserved.
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
%% Description: Implements base 64 encode and decode. See RFC4648.

-module(nuid_base64).

-export([encode/1, decode/1]).

%% RFC 4648§5: base64url - Base 64 Encoding alphabet
%-type base64_alphabet() :: $0..$9 | $- | $A..$Z | $_ | $a..$z.
%% Alphabet reordered to be lexicographically ordered.

%% The following type is a subtype of string() for return values
%% of encoding functions.
-type base64_binary() :: binary().

-spec encode(Data) -> Base64 when
    Data :: binary(),
    Base64 :: base64_binary().

encode(Bin) when is_binary(Bin) ->
    encode_binary(Bin, <<>>);
encode(List) when is_list(List) ->
    error({badarg, List}).

encode_binary(<<>>, A) ->
    A;
encode_binary(<<B1:8>>, A) ->
    <<A/bits, (b64e(B1 bsr 2)):8, (b64e((B1 band 3) bsl 4)):8>>;
encode_binary(<<B1:8, B2:8>>, A) ->
    <<A/bits, (b64e(B1 bsr 2)):8, (b64e(((B1 band 3) bsl 4) bor (B2 bsr 4))):8,
        (b64e((B2 band 15) bsl 2)):8>>;
encode_binary(<<B1:8, B2:8, B3:8, Ls/bits>>, A) ->
    BB = (B1 bsl 16) bor (B2 bsl 8) bor B3,
    encode_binary(
        Ls,
        <<A/bits, (b64e(BB bsr 18)):8, (b64e((BB bsr 12) band 63)):8, (b64e((BB bsr 6) band 63)):8,
            (b64e(BB band 63)):8>>
    ).

%% mime_decode strips away all characters not Base64 before
%% converting, whereas decode crashes if an illegal character is found
-spec decode(Base64) -> Data when
    Base64 :: base64_binary(),
    Data :: binary().

decode(Bin) when is_binary(Bin) ->
    decode_binary(Bin, <<>>);
decode(List) when is_list(List) ->
    error({badarg, List}).

decode_binary(<<C1:8, Cs/bits>>, A) ->
    decode_binary(Cs, A, b64d(C1));
decode_binary(<<>>, A) ->
    A.

decode_binary(<<C2:8, Cs/bits>>, A, B1) ->
    decode_binary(Cs, A, B1, b64d(C2)).

decode_binary(<<C3:8, Cs/bits>>, A, B1, B2) ->
    decode_binary(Cs, A, B1, B2, b64d(C3));
decode_binary(<<>>, A, B1, B2) ->
    <<A/bits, B1:6, (B2 bsr 4):2>>.

decode_binary(<<C4:8, Cs/bits>>, A, B1, B2, B3) ->
    B4 = b64d(C4),
    decode_binary(Cs, <<A/bits, B1:6, B2:6, B3:6, B4:6>>);
decode_binary(<<>>, A, B1, B2, B3) ->
    <<A/bits, B1:6, B2:6, (B3 bsr 2):4>>.

%%%========================================================================
%%% Internal functions
%%%========================================================================

%% accessors
-compile({inline, [{b64d, 1}]}).
%% One-based decode map.
b64d(X) ->
    element(
        X,
        {bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
            bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
            bad, bad, bad, bad, bad, bad, bad, bad, bad, 0, bad, bad, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            bad, bad, bad, bad, bad, bad, bad, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
            24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, bad, bad, bad, bad, 37, bad, 38, 39,
            40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61,
            62, 63, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
            bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
            bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
            bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
            bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
            bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
            bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
            bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad}
    ).

-compile({inline, [{b64e, 1}]}).
b64e(X) ->
    element(
        X + 1,
        {
            $-,
            $0,
            $1,
            $2,
            $3,
            $4,
            $5,
            $6,
            $7,
            $8,
            $9,
            $A,
            $B,
            $C,
            $D,
            $E,
            $F,
            $G,
            $H,
            $I,
            $J,
            $K,
            $L,
            $M,
            $N,
            $O,
            $P,
            $Q,
            $R,
            $S,
            $T,
            $U,
            $V,
            $W,
            $X,
            $Y,
            $Z,
            $_,
            $a,
            $b,
            $c,
            $d,
            $e,
            $f,
            $g,
            $h,
            $i,
            $j,
            $k,
            $l,
            $m,
            $n,
            $o,
            $p,
            $q,
            $r,
            $s,
            $t,
            $u,
            $v,
            $w,
            $x,
            $y,
            $z
        }
    ).

%%-----------------------------------------------------------------------
%% Code to generate decode table
%%-----------------------------------------------------------------------
%% code({value, {Pos, _Value}}) ->
%%     Pos;
%% code(_) ->
%%     bad.
%% 
%% alphabet_pos([], _Pos, Acc) ->
%%     lists:reverse(Acc);
%% alphabet_pos([Char | Rest], Pos, Acc) ->
%%     alphabet_pos(Rest, Pos + 1, [{Pos, Char} | Acc]).
%% 
%% decode_tuple(AlphabetPos) ->
%%     Seq = lists:seq(1, 256),
%%     decode_tuple(AlphabetPos, Seq, []).
%% 
%% 
%% decode_tuple(_AlphabetPos, [], Acc) ->
%%     list_to_tuple(lists:reverse(Acc));
%% decode_tuple(AlphabetPos, [Char | Rest], Acc) ->
%%     Value = code(lists:keysearch(Char, 2, AlphabetPos)),
%%     decode_tuple(AlphabetPos, Rest, [Value | Acc]).
%% 
%% decode_table() ->
%%     Alphabet = [$-, $0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
%%          $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N,
%%          $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
%%          $_, $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n,
%%          $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z],
%%     AlphabetPos = alphabet_pos(Alphabet, 0, []),
%%     decode_tuple(AlphabetPos).
