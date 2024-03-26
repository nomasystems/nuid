# nuid
[![nuid](https://github.com/nomasystems/nuid/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/nuid/actions/workflows/ci.yml)

`nuid` is an OTP library to generate unique identifiers.

## Setup

Add `nuid` to your project dependencies.

```erl
%%% e.g., rebar.config
{deps, [
    {nuid, {git, "git@github.com:nomasystems/nuid.git", {branch, "main"}}}
]}.
```

## Features

Traditionally we've been using uuid1 as defined on [rfc4122](https://datatracker.ietf.org/doc/html/rfc4122) and later we transitioned to uuid6 as defined on [draft-peabody-dispatch-new-uuid-format](https://datatracker.ietf.org/doc/html/draft-peabody-dispatch-new-uuid-format).

The latest is preferred since it can be ordered by the time of creation.

Recently we had the necessity of having identifiers with more entropy. Specifically, after a security revision, we've been requested to use at least the same random bits as uuid4 (122). With that in mind, we started searching for standards covering these requirements:

Must be able to order identifiers by the time of creation.
Identifiers must have at least 122 bits of cryptographically strong random numbers.

- Some optional requirements would make the identifiers more appealing:
- Being unique (at least having a low collision probability).
- Carry information about where the identifier has been created.

We found these two great de-facto standards: 
- [ulid](https://github.com/ulid/spec)
- [ksuid](https://github.com/segmentio/ksuid)

But they lack the second requirement, the one about 122 bits of cryptographically strong random numbers. So we came up with nuid2. 

nuid2 has these properties:

- Lexicographically sortable.
- Sixteen cryptographically strong random bytes. (128bits)
- It is unique (at least has a low collision probability).
- It carries 3 bytes of information reserved for origin information.
- No longer than uuid (36 bytes)
- URL safe


```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                    POSIX time in seconds                      |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                    Unique sortable integer                    |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|        Origin information (node)                |             |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+             +
|                                                               |
+                                                               +
|          Cryptographically strong random bits (128)           |
+                                                               +
|                                                               |
+                                                 +-+-+-+-+-+-+-+
|                                                 |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

              Figure 1: nuid2 Field and bit Layout
```

We encode this information on what we'll call base64'. That is a url safe, sortable base64
representation.


This is base64' alphabet in order:

```
-, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
_,
a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z
```

We took Erlang/OTP base64 module and modified it to meet these properties.
It's a pretty amazing code. Thanks.

We also came up with nuid1, having just these properties

- Lexicographically sortable. It can be used on systems that uuid6 were used. All nuid1 identifiers generated after a uuid6 has been generated will be lexicographically greater.
- 16 cryptographically strong random bytes. (128bits)
- It is unique (at least has a low collision probability).


```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|    Hex unique time in us  |-|       base64' 16 random bytes             |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

              Figure 2: nuid1 Field and Byte Layout

(*) where dash at byte 14 is the 45 ASCII character

```


## API
`nuid` exposes utilities via its API that allows you to:

| Function | Description |
| --------  | ------------ |
| `nuid:uuid1/0` | Generates RFC 4122 uuid v1 |
| `nuid:uuid3/2` | Generates RFC 4122 uuid v3 |
| `nuid:uuid4/0` | Generates RFC 4122 uuid v4 |
| `nuid:uuid5/2` | Generates RFC 4122 uuid v5 |
| `nuid:uuid6/0` | Generates draft-ietf-uuidrev-rfc4122bis uuid v6 |
| `nuid:uuid7/0` | Generates draft-ietf-uuidrev-rfc4122bis uuid v7 |
| `nuid:uuid8/1` | Generates draft-ietf-uuidrev-rfc4122bis uuid v8 |
| `nuid:uuid8/3` | Generates draft-ietf-uuidrev-rfc4122bis uuid v8 |
| `nuid:nil_uuid/0` | Generates RFC 4122 nil uuid |
| `nuid:max_uuid/0` | Generates draft-ietf-uuidrev-rfc4122bis max uuid |
| `nuid:uuid1_info/1` | Gets info from uuid1 (generation time, node, unique) |
| `nuid:uuid6_info/1` | Gets info from uuid6 (generation time, node, unique) |
| `nuid:uuid7_info/1` | Gets info from uuid7 (generation time) |
| `nuid:nuid1/0` | Generates Noma nuid1 |
| `nuid:nuid2/0` | Generates Noma nuid2 |
| `nuid:nuid1_info/1` | Gets info from nuid1 (generation time) |
| `nuid:nuid2_info/1` | Gets info from nuid2 (generation time, node) |


## Implementation


## A simple example

```erl
1> nuid:uuid1().
<<"07e826fe-ed86-1060-8000-00001430cc44">>
2> nuid:uuid3(url, <<"https://www.nomasystems.com">>).
<<"67805345-d49e-3058-9ad3-160686e8ee2a">>
3> nuid:uuid4().                                      
<<"37a9e737-f680-44a9-b83d-a517ec758b75">>
4> nuid:uuid5(dns, <<"nomasystems.com">>).
<<"cefe05b2-95ca-5b0a-ad06-9b3f2b38e532">>
5> nuid:uuid6().                         
<<"0607e826-ff71-6410-8000-00002430cc44">>
6> nuid:uuid7().
<<"018b3d7a-9f9a-7577-adb2-08761e3d87f7">>
7> nuid:uuid8(<<16#f, 16#e, 16#d, 16#c, 16#b, 16#a, 16#9, 16#8, 16#7, 16#6, 16#5, 16#4, 16#3, 16#2, 16#1, 16#0>>).
<<"0f0e0d0c-0b0a-8908-8706-050403020100">>
8> nuid:nuid1().
<<"607e826ff7388-4WX7g2peZpWw9QAIpkRp-F">>
9> nuid:nuid2().
<<"OHtpP-----Fkn3F6JaT5Kxnm_NAiDzFgGMzc">>
10> nuid:uuid1_info(nuid:uuid1()).
{uuidInfo,{{2023,10,17},{11,52,8}},5,nonode@nohost}
11> nuid:uuid6_info(nuid:uuid6()).
{uuidInfo,{{2023,10,17},{11,52,8}},6,nonode@nohost}
12> nuid:nuid1_info(nuid:nuid1()).
{{2023,10,17},{11,52,8}}
13> nuid:nuid2_info(nuid:nuid2()).
{nonode@nohost,{{2023,10,17},{11,52,8}}}
14> nuid:nil_uuid(nuid:nuid2()).
<<"00000000-0000-0000-0000-000000000000">>
15> nuid:max_uuid(nuid:nuid2()).
<<"FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF">>.
```

## Benchmarks

Run a rebar3 shell using the `bench` profile:
```sh
rebar3 as bench shell
```
Run the following command:
```erl
1> nuid_bench:bench().
```

This benchmark compares different unique identifiers generated by `nuid`.

### Results

```
nuid1 creation time: 1.83 (us)
nuid2 creation time: 1.55 (us)
uuid1 creation time: 0.92 (us)
uuid4 creation time: 1.69 (us)
uuid6 creation time: 0.91 (us)
uuid7 creation time: 1.82 (us)
```

## Support

Any doubt or suggestion? Please, check out [our issue tracker](https://github.com/nomasystems/nuid/issues).
