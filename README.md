# capnp-fortran

[![codecov](https://codecov.io/gh/HaoZeke/capnp-fortran/graph/badge.svg)](https://codecov.io/gh/HaoZeke/capnp-fortran)

A native modern-Fortran (F2018) implementation of [Cap'n Proto](https://capnproto.org)
serialization: the wire format runtime, stream framing, the packed codec,
canonicalization, and a `capnpc-fortran` schema compiler backend. No C
library underneath; only `iso_fortran_env` kinds and, for the optional C API,
`iso_c_binding`.

Documentation: **https://capnp-fortran.rgoswami.me** (Sphinx site; sources under `docs/`).
Local docs: `pixi run -e docs docs`. Contributing: [CONTRIBUTING.md](CONTRIBUTING.md). Security: [SECURITY.md](SECURITY.md). Linkcheck: `pixi run -e docs linkcheck` (needs `lychee` on PATH). Hooks: `prek install` then `prek run -a` (config in `prek.toml`; version bumps via `cog bump`).

## Features

- Full wire-format runtime: struct/list/far/double-far/capability pointers,
  growable segment arena, default-XOR field access, Text/Data, primitive,
  bit, pointer, and composite lists, traversal and depth guards.
- Schema-evolution semantics matching the C++ implementation: reads past a
  struct's data section return defaults, primitive lists upgrade to struct
  views (element as field `@0`), composite lists downgrade to primitive or
  pointer views.
- Deep copy between messages; cross-message `capnp_setp` clones, as C++
  `set()` does.
- Serialization: stream segment table, whole-buffer packed codec,
  incremental pack and unpack for chunked I/O, zero-copy segment and
  Text/Data views, orphans (disown/adopt), file helpers.
- Canonical form (`capnp convert binary:canonical` byte-parity, verified in
  the test suite).
- `capnpc-fortran`: a `capnp compile -o` plugin, self-hosted on this runtime
  (it reads `CodeGeneratorRequest` with hand-rolled accessors, the same
  bootstrap capnpc-c uses). Generates one module per schema file: handle
  types, accessors with scalar defaults, Text/Data/struct/list pointer
  defaults embedded as blobs, enums, constants, unions and groups,
  cross-file imports, `List(Text)`/`List(Data)` element helpers, anyPointer.
- A `bind(c)` shim (`capnp_cabi`) and a cmocka golden-master tier that
  builds identical messages with this runtime and with
  [c-capnproto](https://github.com/HaoZeke/c-capnproto), comparing
  wire bytes (see `interop/README.md`).
- RPC level 1: two-party vat over a POSIX socket transport
  (`capnp_rpc`, `capnp_posix`, `capnp_rpc_transport`), cap tables,
  promise pipelining, embargo echo (same-process fpm suite), and level 2
  persistence hooks; bootstrap and pipelined/settled calls are also
  protocol-tested against a live capnp-C++ peer when that interop tier is built.
- RPC level 4: `Join`, answering whether a set of capabilities names one
  object, per the two-party rules in `rpc-twoparty.capnp`.

## Parity

Feature coverage against the two reference serialization implementations,
capnp-c ([c-capnproto](https://github.com/HaoZeke/c-capnproto))
and capnp-C++:

| Feature | capnp-c | capnp-C++ | capnp-fortran |
|---------|---------|-----------|---------------|
| Wire format read/write (all pointer kinds) | yes | yes | yes |
| Stream framing | yes | yes | yes |
| Packed codec | yes | yes | yes, plus incremental pack/unpack |
| Zero-copy reads from a caller buffer | yes | yes | yes (`capnp_deserialize_view`, `capnp_get_data_view`) |
| Traversal and depth limits | yes (0.2+) | yes | yes |
| Schema-evolution reads (defaults past end, list up/downgrades) | partial | yes | yes |
| Deep copy / cross-message set | no | yes | yes |
| Orphans (disown/adopt) | no | yes | yes |
| Canonical form | yes (0.3.0) | yes | yes (byte-parity tested) |
| Code generator plugin (`capnp compile -o`) | yes | yes | yes (`capnpc-fortran`) |
| Generated: unions, groups, defaults, imports, constants incl. pointer-valued | yes | yes | yes |
| Capability pointers on the wire | yes | yes | yes |
| RPC level 1 (calls, cap tables, promise pipelining, embargo echo) | no | yes | yes (`capnp_rpc`, two-party) |
| RPC level 2 (persistence hooks) | no | partial | hooks (`RPC_PERSISTENT_IFACE`, app-defined SturdyRefs) |
| RPC level 3 (three-party: `Provide`/`Accept`) | no | yes (post-1.4.0) | yes (`rpc-threeparty.capnp` network layer) |
| RPC level 4 (`Join`, reference equality) | no | no | yes, two-party (upstream C++ has none) |
| `-> stream` flow control | no | yes | yes (`rpc_stream_t`, windowed) |
| Typed interface stubs in generated code | no | yes | yes (client helpers + abstract server base) |
| Generics in generated code | no | yes | brand-resolved instantiations (direct, list-element, list-binding, nested) |
| Dynamic reflection API | no | yes | yes (`capnp_dynamic`, by-name read/write) |

The RPC tier is protocol-tested against a live capnp-C++ (`libcapnp-rpc`)
peer in the interop suite: the Fortran vat bootstraps an `EzRpcServer`
over TCP and calls it, pipelined and settled.

Level 3 is three-party introduction. `rpc.capnp` leaves `ProvisionId`,
`RecipientId` and `ThirdPartyCapId` as `AnyPointer` for the network layer
to define, and `rpc-twoparty.capnp` declares them empty, "never used,
because there is no third party". Defining them is the network layer's
job, so this family ships one: `schema/rpc-threeparty.capnp`, shared
verbatim by all four ports, names a vat by host and port and keys a
handoff by a nonce.

Alice, holding a capability hosted by Bob, wants Carol to have it. She
sends Bob `Provide{target, recipient = RecipientId{carol, nonce}}` and
tells Carol where to go; Carol sends Bob `Accept{ProvisionId{nonce}}` and
gets the capability back in the Return. The nonce is the whole of the
arrangement, which is what lets Bob hand the capability over without
taking Carol's word for who sent her, and it is single-use: leaving it
claimable would let anyone who learned it take the capability again.

The level 3 tests decode frames the reference `capnp` CLI encoded
(`test/fixtures/rpc-{provide,accept}.bin`, regenerated by
`scripts/gen-rpc-frames.sh`), not only frames this library built: a
layout the vat shares with its own builder but the wire format does not
would pass every self-built test.

Upstream added level 3 after 1.4.0; `capnp` 1.4.0 has no `Provide` /
`Accept` handling at all, and main (4bfab51) has it.

Level 4 is `Join`: asking whether several capabilities are one object.
The two-party network defines this fully, and it is implemented here.
Parts accumulate against their `joinId`, and every part is answered only
once the whole set has arrived, since no single part is answerable
alone; equal export ids then mean the same object, and exactly one
`JoinResult` carries the joined capability. Upstream C++ has no `Join`
case at all -- it falls through to the default branch and replies
`unimplemented` -- so this is the one place the family is ahead of the
reference implementation rather than catching up to it. Generics and
reflection sit outside both this project's and capnpc-c's generated-code
scope; the wire format carries generic types either way, so messages
produced by C++ users of those features still read correctly here.

## Changelog

See [CHANGELOG.md](CHANGELOG.md) for release notes.

## Install

With [fpm](https://fpm.fortran-lang.org), to build this repository:

```console
$ fpm build
$ fpm test
```

To depend on it, name the git source and a tag in your own `fpm.toml`:

```toml
[dependencies]
capnp = { git = "https://github.com/HaoZeke/capnp-fortran", tag = "v0.1.1" }
```

then `use capnp` and build as usual. This is the supported route:
`fortran-lang/fpm-registry` has merged nothing since December 2021, so a
registry name is not available to depend on.

Toolchain (gfortran, fpm, fypp, the `capnp` tool) is pinned in `pixi.toml`:

```console
$ pixi install
$ pixi run build
$ pixi run test
```

### CMake / FetchContent

Official Cap'n Proto C++ uses `find_package(CapnProto)` and `CapnProto::capnp`.
This Fortran port uses a **different** CMake project/namespace so the two can
coexist:

```cmake
include(FetchContent)
FetchContent_Declare(
  capnp_fortran
  GIT_REPOSITORY https://github.com/HaoZeke/capnp-fortran.git
  GIT_TAG        v0.1.1
)
FetchContent_MakeAvailable(capnp_fortran)
target_link_libraries(myapp PRIVATE capnp_fortran::capnp_fortran)
```

Fortran code still does `use capnp` (module API). CMake target is
`capnp_fortran::capnp_fortran`; package config is `find_package(capnp_fortran)`.

Standalone: `cmake -S . -B build-cmake && cmake --build build-cmake`. Options:
`CAPNP_FORTRAN_BUILD_PLUGIN`, `CAPNP_FORTRAN_BUILD_SHARED`, `CAPNP_FORTRAN_INSTALL`.
Details: [Install docs](https://capnp-fortran.rgoswami.me/install).

### Coverage

```console
$ pixi run -e coverage coverage   # gfortran --coverage → coverage-report/
```

CI (`.github/workflows/coverage.yml`):

- GitHub Actions **artifact** + job summary + sticky **PR comment** (works with no SaaS)
- **[Codecov](https://codecov.io)** upload (free for public repos): add secret `CODECOV_TOKEN`
  from [app.codecov.io](https://app.codecov.io) → repo settings. Without it the step fails soft.

```markdown
[![codecov](https://codecov.io/gh/HaoZeke/capnp-fortran/graph/badge.svg)](https://codecov.io/gh/HaoZeke/capnp-fortran)
```

## Tutorial: write and read a message

Compile a schema:

```console
$ capnp compile -o build/gfortran_*/app/capnpc-fortran schema/addressbook.capnp
```

This writes `addressbook_capnp.f90`. Build a message and read it back:

```fortran
program tutorial
   use capnp
   use addressbook_capnp
   implicit none
   type(capnp_message_t), target :: msg, rmsg
   type(address_book_t) :: book
   type(person_t) :: alice
   type(capnp_ptr_t) :: people
   integer(int8), allocatable :: bytes(:)
   character(len=:), allocatable :: name
   integer :: err

   call capnp_message_init_builder(msg, err)
   book = address_book_new_root(msg, err)
   people = address_book_people_init(book, 1_int64, err)
   alice%p = capnp_list_get_struct(people, 0, err)
   call person_id_set(alice, 123_int64, err)
   call person_name_set(alice, 'Alice', err)
   call capnp_serialize_bytes(msg, bytes, err)

   call capnp_deserialize_bytes(bytes, rmsg, err)
   book = address_book_read_root(rmsg, err)
   people = address_book_people_get(book, err)
   alice%p = capnp_list_get_struct(people, 0, err)
   call person_name_get(alice, name, err)
   print '(a)', name   ! Alice
end program tutorial
```

Messages carry `target` because handles hold a pointer to their message.
Every fallible call returns an `err` code (`CAPNP_OK` on success); readers
never crash on malformed input, they return errors and defaults.

## Layout

| Path | Contents |
|------|----------|
| `src/` | Runtime modules; `capnp_endian`/`capnp_message` generate from `.fypp` (`pixi run gen`) |
| `app/` | `capnpc-fortran` plugin (schema reader + emitter) |
| `schema/`, `test/fixtures/` | Fixture schemas and `capnp`-tool golden bytes |
| `test/` | fpm test programs, including generated-code and interop decoding tests |
| `interop/`, `meson.build` | cmocka golden-master tier against c-capnproto |
| `docs/` | Sphinx site (org → rst); live at https://capnp-fortran.rgoswami.me |
| `CMakeLists.txt`, `cmake/` | FetchContent / `capnp_fortran::capnp_fortran` (not CapnProto::) |

## Family

| Tree | Product |
|------|---------|
| C | [c-capnproto](https://github.com/HaoZeke/c-capnproto) |
| Janet | [capnp-janet](https://github.com/HaoZeke/capnp-janet) |
| TypeScript | [capnp-ts](https://github.com/HaoZeke/capnp-ts) |

All four share `schema/rpc-threeparty.capnp`, the level 3 network layer,
and the reference frames encoded from it.

## Citation

Cite as: Rohit Goswami, *capnp-fortran: a native Fortran Cap'n Proto
implementation*, 2026. Machine-readable metadata is in
[`CITATION.cff`](CITATION.cff).

## License

[MIT](LICENSE).
