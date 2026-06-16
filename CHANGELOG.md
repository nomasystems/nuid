# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2026-06-15

Initial public release.

### Added

- RFC 4122 UUIDs: versions 1, 3 (MD5), 4, and 5 (SHA-1)
- RFC 9562 UUIDs: versions 6, 7, and 8
- Nil and max UUIDs
- `nuid1`: hex-timestamped identifier with 128 bits of randomness, sortable and ordered after RFC 9562 version 6 UUIDs
- `nuid2`: timestamped, node-tagged identifier with 128 bits of randomness, sortable and URL-safe
- Sortable, URL-safe base64 codec (`nuid_base64`)
- Time and node recovery via `uuid1_info/1`, `uuid6_info/1`, `uuid7_info/1`, `nuid1_info/1`, `nuid2_info/1`
- Property-based test suites backed by triq
