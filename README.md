# capnp-fortran

A native modern-Fortran (F2018) implementation of [Cap'n Proto](https://capnproto.org)
serialization: the wire format runtime, stream framing, the packed codec,
canonicalization, and a `capnpc-fortran` schema compiler backend. No C
library underneath; only `iso_fortran_env` kinds and, for the optional C API,
`iso_c_binding`.

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
- Serialization: stream segment table, whole-buffer packed codec, an
  incremental unpacker for chunked input, file helpers.
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
  [c-capnproto](https://github.com/opensourcerouting/c-capnproto), comparing
  wire bytes (see `interop/README.md`).
- RPC level 1: two-party vat over a POSIX socket transport
  (`capnp_rpc`, `capnp_posix`, `capnp_rpc_transport`), cap tables,
  promise pipelining, embargo echo, and level 2 persistence hooks;
  protocol-tested against a live capnp-C++ peer.

## Parity

Feature coverage against the two reference serialization implementations,
capnp-c ([c-capnproto](https://github.com/opensourcerouting/c-capnproto))
and capnp-C++:

| Feature | capnp-c | capnp-C++ | capnp-fortran |
|---------|---------|-----------|---------------|
| Wire format read/write (all pointer kinds) | yes | yes | yes |
| Stream framing | yes | yes | yes |
| Packed codec | yes | yes | yes, plus incremental pack/unpack |
| Zero-copy reads from a caller buffer | yes | yes | yes (`capnp_deserialize_view`, `capnp_get_data_view`) |
| Traversal and depth limits | no | yes | yes |
| Schema-evolution reads (defaults past end, list up/downgrades) | partial | yes | yes |
| Deep copy / cross-message set | no | yes | yes |
| Orphans (disown/adopt) | no | yes | yes |
| Canonical form | no | yes | yes (byte-parity tested) |
| Code generator plugin (`capnp compile -o`) | yes | yes | yes (`capnpc-fortran`) |
| Generated: unions, groups, defaults, imports, constants incl. pointer-valued | yes | yes | yes |
| Capability pointers on the wire | yes | yes | yes |
| RPC level 1 (calls, cap tables, promise pipelining, embargo echo) | no | yes | yes (`capnp_rpc`, two-party) |
| RPC level 2 (persistence hooks) | no | partial | hooks (`RPC_PERSISTENT_IFACE`, app-defined SturdyRefs) |
| RPC level 3/4 (three-party, joins) | no | no (replies `unimplemented`) | no (replies `unimplemented`, same as C++) |
| `-> stream` flow control | no | yes | yes (`rpc_stream_t`, windowed) |
| Typed interface stubs in generated code | no | yes | yes (client helpers + abstract server base) |
| Generics in generated code | no | yes | brand-resolved instantiations (direct, list-element, list-binding, nested) |
| Dynamic reflection API | no | yes | yes (`capnp_dynamic`, by-name read/write) |

The RPC tier is protocol-tested against a live capnp-C++ (`libcapnp-rpc`)
peer in the interop suite: the Fortran vat bootstraps an `EzRpcServer`
over TCP and calls it, pipelined and settled. No implementation of any
language reaches RPC levels 3-4; like C++, this vat answers those
messages with `Message.unimplemented` as the spec requires. Generics and
reflection sit outside both this project's and capnpc-c's generated-code
scope; the wire format carries generic types either way, so messages
produced by C++ users of those features still read correctly here.

## Install

With [fpm](https://fpm.fortran-lang.org):

```console
$ fpm build
$ fpm test
```

Toolchain (gfortran, fpm, fypp, the `capnp` tool) is pinned in `pixi.toml`:

```console
$ pixi install
$ pixi run build
$ pixi run test
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
| `docs/reference.md` | full public API reference |

## Citation

Cite as: Rohit Goswami, *capnp-fortran: a native Fortran Cap'n Proto
implementation*, 2026.

## License

[MIT](LICENSE).
