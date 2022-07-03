# nuid
![nuid](https://github.com/nomasystems/nuid/actions/workflows/build.yml/badge.svg)

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
| `nuid:uuid6/0` | Generates draft-peabody-dispatch-new-uuid-format-04 uuid v6 |
| `nuid:uuid7/0` | Generates draft-peabody-dispatch-new-uuid-format-04 uuid v6 |
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
<<"e2dfd6de-1a8c-1050-8000-00001430cc44">>
2> nuid:uuid3(url, <<"https://www.nomasystems.com">>).
<<"67805345-d49e-3058-9ad3-160686e8ee2a">>
3> nuid:uuid4().                                      
<<"c0a072d8-a60d-4f8b-9da8-a03205447f55">>
4> nuid:uuid5(dns, <<"nomasystems.com">>).
<<"cefe05b2-95ca-5b0a-ad06-9b3f2b38e532">>
5> nuid:uuid6().                         
<<"05e2dfda-f616-6b60-8000-00002430cc44">>
6> nuid:uuid7().
<<"0181c286-10cb-7083-a53c-7bd6c4ef7bab">>
7> nuid:nuid1().
<<"5e2dfdc12ef84-wUKoLPkJLj77JtNVoC3oJ-">>
8> nuid:nuid2().
<<"Ng3cUk----Fkn3GRj5J0gLhQOgAkc8C-It88">>
9> nuid:uuid1_info(nuid:uuid1()).
{uuidInfo,{{2022,7,3},{5,26,59}},5,nonode@nohost}
10> nuid:uuid6_info(nuid:uuid6()).
{uuidInfo,{{2022,7,3},{5,27,12}},6,nonode@nohost}
11> nuid:nuid1_info(nuid:nuid1()).
{{2022,7,3},{5,27,28}}
12> nuid:nuid2_info(nuid:nuid2()).
{nonode@nohost,{{2022,7,3},{5,27,34}}}
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
