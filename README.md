# capnp-fortran

[![codecov](https://codecov.io/gh/HaoZeke/capnp-fortran/graph/badge.svg)](https://codecov.io/gh/HaoZeke/capnp-fortran)

A native modern-Fortran (F2018) implementation of [Cap'n Proto](https://capnproto.org)
serialization: the wire format runtime, stream framing, the packed codec,
canonicalization, and a `capnpc-fortran` schema compiler backend. No C
library underneath; only `iso_fortran_env` kinds and, for the optional C API,
`iso_c_binding`.

Documentation: **https://capnp-fortran.rgoswami.me** (Sphinx site; sources under `docs/`).
Local docs: `pixi run -e docs docs`. Contributing: [CONTRIBUTING.md](CONTRIBUTING.md). Security: [SECURITY.md](SECURITY.md). Linkcheck: `pixi run -e docs linkcheck` (needs `lychee` on PATH). Hooks: `prek install` then `prek run -a` (config in `prek.toml`; version bumps via `python3 scripts/cog_bump.py`).

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
  cross-file imports, schema annotations, `List(Text)`/`List(Data)` element
  helpers, anyPointer.
- A `bind(c)` shim (`capnp_cabi`) and a cmocka golden-master tier that
  builds identical messages with this runtime and with
  [c-capnproto](https://github.com/HaoZeke/c-capnproto), comparing
  wire bytes (see `interop/README.md`).
- RPC level 1: two-party vat over a POSIX socket transport
  and pluggable byte streams (`capnp_rpc`, `capnp_posix`,
  `capnp_rpc_transport`), cap tables,
  promise pipelining, embargo echo (same-process fpm suite), and level 2
  `Persistent<SturdyRef, Owner>.save` calls; bootstrap and
  pipelined/settled calls are also
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
| Stream framing | yes | yes | yes (pluggable byte stream, fd and socketpair adapters) |
| Packed codec | yes | yes | yes, plus incremental pack/unpack |
| Zero-copy reads from a caller buffer | yes | yes | yes (`capnp_deserialize_view`, `capnp_get_data_view`) |
| Traversal and depth limits | yes (0.2+) | yes | yes |
| Schema-evolution reads (defaults past end, list up/downgrades) | partial | yes | yes |
| Deep copy / cross-message set | no | yes | yes |
| Orphans (disown/adopt) | no | yes | yes |
| Canonical form | yes (0.3.0) | yes | yes (byte-parity tested) |
| Code generator plugin (`capnp compile -o`) | yes | yes | yes (`capnpc-fortran`) |
| Generated: unions, groups, defaults, imports, constants incl. pointer-valued | yes | yes | yes |
| Generated annotations and reflection metadata | no | yes | partial (annotation constants only) |
| Capability pointers on the wire | yes | yes | yes |
| RPC level 1 (calls, cap tables, promise pipelining, embargo echo) | no | yes | yes (`capnp_rpc`, two-party) |
| RPC level 2 (`Persistent.save`) | no | partial | yes (`rpc_persistent_save_begin` / `_wait`, app-defined SturdyRef and Owner) |
| RPC level 3 (three-party: `Provide`/`Accept`, vines, embargo) | no | post-1.4.0 only | yes (`rpc-threeparty.capnp`, VatId dialing, live three-vat TCP handoff) |
| RPC level 4 (`Join`, reference equality) | no | no | yes (two-party sender and receiver; C++ replies unimplemented) |
| `-> stream` flow control and cancellation | no | yes | yes (`rpc_stream_t`, windowed, Finish cancellation) |
| Capability membranes | no | yes | partial (call checks, shared revocation, recursive result caps, unwrap on return) |
| Typed interface stubs in generated code | no | yes | partial (client helpers + abstract server base) |
| Generics in generated code | no | yes | partial (tested brand-resolved instantiations) |
| Dynamic reflection API | no | yes | partial (named scalar, text, and pointer operations) |

The RPC tier is protocol-tested against a live capnp-C++ (`libcapnp-rpc`)
peer in the interop suite: the Fortran vat bootstraps an `EzRpcServer`
over TCP and calls it, pipelined and settled.

`rpc_byte_stream_t` is the transport boundary used by RPC connections.
Applications can implement its exact-read, exact-write, readiness, and
close bindings for another stream. `rpc_fd_stream_t` adapts an existing
descriptor, and `rpc_socketpair_streams` creates an owned loopback pair.
TCP remains an adapter choice rather than a requirement of the RPC
core. TLS can be supplied by a stream implementation; the library does
not impose a TLS stack or certificate policy.

Level 2 follows `capnp/persistent.capnp`: `rpc_persistent_save_begin`
sends `Persistent<AnyPointer, AnyPointer>.save`, including the optional
`SaveParams.sealFor` owner, and `rpc_persistent_save_wait` returns
`SaveResults.sturdyRef`. Both values are realm-defined. Capnp-C++ does
not define a universal restoration protocol or token format, so
restoring a SturdyRef remains an application service rather than an RPC
runtime operation in both implementations.

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

A handoff is arranged on one connection and claimed on another, so the
arrangement belongs to the vat rather than the connection:
`rpc_conn_set_vat` shares it between a vat's connections, and it holds
the capability rather than an export id, since ids are per-connection.
`test/test_rpc_handoff.f90` runs the whole flow across three vats.
Connections given no vat get one to themselves, which is what a
two-party deployment wants.

Both halves of the introduction are here. Hosting is `Provide` and
`Accept`, above. Receiving is the `thirdPartyHosted` CapDescriptor: a
payload can name a capability that lives in a third vat and hand over a
vine, an ordinary import through the introducer, so calls work before
the pickup ever happens. That is the fallback the spec gives receivers
that cannot reach a third party, and it is why the vine must not be
released until the pickup succeeds. `rpc_pending_introductions` exposes
the network-layer VatId, `rpc_introduction_dial` opens its numeric IPv4
address, and `rpc_introduction_done` releases the vine after a successful
`Accept`. With no dial hook the introduction is refused, so a peer
cannot choose the address that gets connected. Applications using DNS
or a directory can resolve the VatId before supplying their own
connection adapter, and a hook that allows the host is what permits
`rpc_introduction_dial` to connect. A connection keeps at most
`MAXINTRO_HELD` introductions; past that a descriptor is only its vine,
as in Cap'n Proto C++, and a vine from call params is released once the
call is answered.

`Accept.embargo` is honoured: an embargoed Accept claims the capability
but is not answered until the introducer sends `Disembargo` with
`context.provide` naming its own Provide question. Answering sooner
would let a call sent straight to us overtake one still in flight
through the introducer.

At most 64 questions sit in the fixed table. A question id past that
is still answered, and the answer, including any capability it
published, is kept until Finish. A question id already in use aborts
the connection rather than replacing the earlier answer. A second
Return for a question that already has one aborts with
`Duplicate Return.`. `sendResultsTo` yourself runs the call, holds the
results, and returns `resultsSentElsewhere`; a later return that takes
from that question returns them. A take from any other answer aborts.
A third party tail call aborts the connection. A `Resolve` for a
promise import adopts the replacement. When that replacement is a
capability this vat hosts and the promise has been called, the vat
sends `Disembargo` with `senderLoopback` and accepts only that embargo
id back.

The level 3 tests decode frames the reference `capnp` CLI encoded
(`test/fixtures/rpc-{provide,accept,introduce}.bin`, regenerated by
`scripts/gen-rpc-frames.sh`), not only frames this library built: a
layout the vat shares with its own builder but the wire format does not
would pass every self-built test.

Capnp-C++ 1.4.0 implements `TwoPartyVatNetwork`, whose
`rpc-twoparty.capnp` IDs intentionally cannot name a third party. It has
no `Provide` or `Accept` handling, so no live 1.4.0 level 3 peer test is
possible. Upstream added level 3 later. This project's
`rpc-threeparty.capnp` is a separate network parameterization rather
than a claim that it interoperates with C++ 1.4.0 three-party routing.

Level 4 is `Join`: asking whether several capabilities are one object.
The two-party network defines this fully, and it is implemented here.
Parts accumulate against their `joinId`, and every part is answered only
once the whole set has arrived, since no single part is answerable
alone. Eight joins can be in flight, and a part that never finishes
holds its question id until Finish. Equal export ids then mean the same
object, and exactly one
`JoinResult` carries the joined capability. `rpc_join_send` emits the
complete set before `rpc_join_wait` collects and settles that one
capability, so equality checks can cross a two-party connection rather
than being limited to local export ids. Capnp-C++ 1.4.0 has no `Join`
case at all and replies `unimplemented`, so no live C++ Join interop
test exists. Generics are generated as brand-resolved instantiations,
and reflection is the dynamic API; the generated reflection metadata is
annotation constants only. capnpc-c generates neither. The wire format
carries generic types either way, so messages produced by C++ users of
those features still read correctly here.

Cancellation uses `Finish` with
`requireEarlyCancellationWorkaround = false`, matching Cap'n Proto 1.0
and newer. `rpc_request_cancellation` abandons one question and
`rpc_stream_cancel` applies the same request to every outstanding stream
call. A callee may still run application code to completion unless its
method opts into cancellation, matching C++'s `allowCancellation`
annotation. The live C++ interop peer includes an opted-in method whose
never-ending promise is dropped by the Fortran cancellation request.

Membranes cover part of `capnp/membrane.h`. A policy can allow or reject
an inbound or outbound call. Wrappers that share a policy share one
revocation flag. Result capabilities are wrapped recursively, and a
wrapper that crosses back through the same policy is unwrapped.
`rpc_membrane_cap` wraps an imported client capability and
`rpc_membrane_server_t` wraps a local server. Redirecting to another
capability, an onRevoked promise that cancels in-flight calls, addRef
policy ownership, parameter-capability membranes, and
shouldResolveBeforeRedirecting are not implemented. Membranes are local
capability machinery, not an RPC wire extension, so their tests run
between Fortran vats rather than against a C++ peer.

## Changelog

See [CHANGELOG.md](CHANGELOG.md) for release notes.

## Install

Install [Pixi](https://pixi.sh/latest/installation/) before running the Pixi
commands when it is not already on `PATH`:

```console
$ curl -fsSL https://pixi.sh/install.sh | bash
$ export PATH="$HOME/.pixi/bin:$PATH"
```

With [fpm](https://fpm.fortran-lang.org), to build this repository:

```console
$ fpm build
$ fpm test
```

To depend on it, name the git source and a tag in your own `fpm.toml`:

```toml
[dependencies]
capnp = { git = "https://github.com/HaoZeke/capnp-fortran", tag = "v0.4.0" }
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

### Meson subproject

Add `subprojects/capnp-fortran.wrap` to a Meson project:

```ini
[wrap-git]
url = https://github.com/HaoZeke/capnp-fortran.git
revision = v0.4.0
```

Then request the dependency and link it:

```meson
capnp_fortran_dep = dependency(
  'capnp-fortran',
  fallback: ['capnp-fortran', 'capnp_fortran_dep'],
)
executable('myapp', 'myapp.f90', dependencies: capnp_fortran_dep)
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
  GIT_TAG        v0.4.0
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

This writes `schema/addressbook_capnp.f90` (the schema's directory is kept). Build a message and read it back:

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
