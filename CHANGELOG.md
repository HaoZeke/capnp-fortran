# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
Pre-1.0 minor releases may include breaking API changes.

## [0.1.0] — 2026-07-08

First public release: a native modern-Fortran (F2018) Cap'n Proto stack
(wire runtime, packed and canonical codecs, `capnpc-fortran` schema plugin,
optional C ABI, and two-party RPC).

### Added

#### Wire runtime

- Struct, list, far, double-far, and capability pointer codecs
- Growable multi-segment arena with traversal and nested depth guards
- Default-XOR field access; Text/Data; primitive, bit, pointer, and composite lists
- Schema-evolution list upgrade/downgrade views (Cap'n Proto C++ semantics)
- Deep copy between messages; cross-message `capnp_setp` clone
- Orphans (disown/adopt); zero-copy segment and Text/Data views
- Stream segment-table framing; whole-buffer and incremental packed codecs
- File helpers; multi-message packed stream reads
- Canonical form with byte-parity against `capnp convert binary:canonical`
- `capnp_total_size` word accounting

#### Schema compiler (`capnpc-fortran`)

- Self-hosted `capnp compile -o` plugin reading `CodeGeneratorRequest`
- Per-schema-file modules: handles, accessors, defaults (including pointer-valued constants as blobs)
- Enums, constants, unions/groups (tag constants), cross-file imports
- `List(Text)` / `List(Data)` element helpers; anyPointer
- Typed interface client stubs and server base types
- Brand-resolved generic instantiations (including nested and list positions)

#### C ABI and interop

- `bind(c)` surface (`capnp_cabi`) aligned with common `capn_*` entry points
- cmocka golden-master tier against vendored [c-capnproto](https://github.com/opensourcerouting/c-capnproto)
- Fixture interop with the official `capnp` encode/decode tool

#### RPC

- Level 1 two-party vat: questions/answers, imports/exports, bootstrap
- POSIX socket transport via `iso_c_binding` (no C sources in the core)
- Promise pipelining; embargo (Disembargo) echo; capability tables
- Level 2 persistence hooks; unimplemented handling for higher levels
- Streaming / flow-control hooks and typed generated RPC clients
- Optional live peer test against capnp-C++ EzRpc when the interop feature is built

#### Dynamic reflection

- SchemaLoader-style dynamic API (`capnp_dynamic`): load CGR, find fields, get/set, named union `which`

#### Tooling and docs

- fpm package `capnp` with executable `capnpc-fortran`
- pixi environments for default, interop, and docs
- GitHub Actions workflows (fpm suite, codegen drift, self-host smoke, interop, big-endian s390x)
- Strict Fortran 2018 conformance script (`scripts/std-check.sh`)
- Sphinx documentation (tutorial, architecture, RPC, interop, API reference)
- README features, parity table, and install/tutorial

### Notes

- Supported development platform for this release: little-endian x86_64 Linux (gfortran).
  A big-endian (s390x) workflow is provided; confirm on your CI after clone.
- Optional C++ RPC peer and cmocka golden master require the `interop` pixi environment
  and system Cap'n Proto / C++ tooling as documented under `interop/`.

[0.1.0]: https://github.com/HaoZeke/capnp-fortran/releases/tag/v0.1.0
