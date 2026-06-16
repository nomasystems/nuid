# nuid

[![Hex.pm](https://img.shields.io/hexpm/v/nuid.svg)](https://hex.pm/packages/nuid)
[![CI](https://github.com/nomasystems/nuid/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/nuid/actions/workflows/ci.yml)

Unique identifier generation for Erlang/OTP 27+: RFC 4122 and RFC 9562 UUIDs, plus sortable `nuid` identifiers.

## Getting started

```erlang
%% rebar.config
{deps, [nuid]}.
```

```erlang
1> nuid:uuid4().
<<"37a9e737-f680-44a9-b83d-a517ec758b75">>
2> nuid:uuid7().
<<"018b3d7a-9f9a-7577-adb2-08761e3d87f7">>
3> nuid:nuid2().
<<"OHtpP-----Fkn3F6JaT5Kxnm_NAiDzFgGMzc">>
4> nuid:uuid7_info(nuid:uuid7()).
{{2023,10,17},{11,52,8}}
```

## Features

- **RFC 4122 UUIDs** versions 1, 3 (MD5), 4, and 5 (SHA-1) ([RFC 4122](https://www.rfc-editor.org/rfc/rfc4122))
- **RFC 9562 UUIDs** versions 6, 7, and 8 ([RFC 9562](https://www.rfc-editor.org/rfc/rfc9562))
- **Nil and max UUIDs**
- **`nuid1` and `nuid2`** sortable identifiers with 128 bits of cryptographically strong randomness
- **Info recovery** creation time and originating node from generated identifiers
- **No dependencies** only OTP `kernel`, `stdlib`, and `crypto`

## API

| Function | Description |
| -------- | ----------- |
| `nuid:uuid1/0` | RFC 4122 UUID v1 |
| `nuid:uuid3/2` | RFC 4122 UUID v3 (MD5, name-based) |
| `nuid:uuid4/0` | RFC 4122 UUID v4 (random) |
| `nuid:uuid5/2` | RFC 4122 UUID v5 (SHA-1, name-based) |
| `nuid:uuid6/0` | RFC 9562 UUID v6 (time-ordered) |
| `nuid:uuid7/0` | RFC 9562 UUID v7 (time-ordered) |
| `nuid:uuid8/1`, `nuid:uuid8/3` | RFC 9562 UUID v8 (vendor-specific) |
| `nuid:nil_uuid/0` | RFC 4122 nil UUID |
| `nuid:max_uuid/0` | RFC 9562 max UUID |
| `nuid:nuid1/0` | `nuid1` identifier |
| `nuid:nuid2/0` | `nuid2` identifier |
| `nuid:uuid1_info/1`, `nuid:uuid6_info/1` | Generation time, node, and counter |
| `nuid:uuid7_info/1`, `nuid:nuid1_info/1` | Generation time |
| `nuid:nuid2_info/1` | Generation time and node |

## The nuid identifiers

We have historically used UUID v1 ([RFC 4122](https://datatracker.ietf.org/doc/html/rfc4122))
and later UUID v6 ([RFC 9562](https://www.rfc-editor.org/rfc/rfc9562)),
preferring v6 because it can be ordered by creation time.

A later security review required identifiers with at least as many
cryptographically strong random bits as UUID v4 (122). The requirements were:

- Orderable by creation time.
- At least 122 bits of cryptographically strong randomness.

With, optionally:

- Uniqueness (or at least a low collision probability).
- Information about where the identifier was created.

The two de facto standards we looked at, [ulid](https://github.com/ulid/spec)
and [ksuid](https://github.com/segmentio/ksuid), do not meet the randomness
requirement, so we defined `nuid2`.

`nuid2` properties:

- Lexicographically sortable.
- Sixteen cryptographically strong random bytes (128 bits).
- Unique (or at least a low collision probability).
- Carries 3 bytes reserved for origin information.
- No longer than a UUID (36 characters).
- URL-safe.

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

              Figure 1: nuid2 field and bit layout
```

We encode this in what we call base64': a URL-safe, sortable base64
representation. Its alphabet, in order:

```
-, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
_,
a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z
```

It is the Erlang/OTP base64 module reordered to preserve byte ordering.

`nuid1` is a lighter identifier with these properties:

- Lexicographically sortable. It can replace UUID v6: every `nuid1`
  generated after a UUID v6 sorts after it.
- Sixteen cryptographically strong random bytes (128 bits).
- Unique (or at least a low collision probability).

```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|    Hex unique time in us  |-|       base64' 16 random bytes             |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

              Figure 2: nuid1 field and byte layout

(*) the dash at byte 14 is ASCII 45
```

## Benchmarks

Run a `rebar3` shell using the `bench` profile:

```sh
rebar3 as bench shell
```

```erlang
1> nuid_bench:bench().
```

This benchmark compares the different identifiers generated by `nuid`.

```
nuid1 creation time: 1.83 (us)
nuid2 creation time: 1.55 (us)
uuid1 creation time: 0.92 (us)
uuid4 creation time: 1.69 (us)
uuid6 creation time: 0.91 (us)
uuid7 creation time: 1.82 (us)
```

## Documentation

[nuid on HexDocs](https://hexdocs.pm/nuid)

## License

Apache License 2.0
