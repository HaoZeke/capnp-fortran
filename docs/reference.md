# API reference

`use capnp` exposes the serialization API: everything below except the C
ABI (`capnp_cabi`) and RPC (`capnp_rpc`, `capnp_posix`,
`capnp_rpc_transport`) sections, which are separate modules used
explicitly. Every fallible operation returns an `err` code; `CAPNP_OK` (0)
means success. Byte buffers are `integer(int8)` arrays, 0-based by
convention. Word offsets and lengths are `integer(int64)`.

## Error codes (`capnp_kinds`)

| Code | Meaning |
|------|---------|
| `CAPNP_OK` | success |
| `CAPNP_ERR_BOUNDS` | access outside a segment |
| `CAPNP_ERR_KIND` | pointer kind mismatch (e.g. list where struct expected) |
| `CAPNP_ERR_DEPTH` | nesting depth limit hit |
| `CAPNP_ERR_TRAVERSAL` | traversal word limit hit |
| `CAPNP_ERR_ALLOC` | builder allocation failure |
| `CAPNP_ERR_FRAMING` | malformed segment table |
| `CAPNP_ERR_PACKED` | malformed packed stream |
| `CAPNP_ERR_ARG` | invalid argument |
| `CAPNP_ERR_SEGMENT` | bad segment id in a far pointer |
| `CAPNP_ERR_IO` | file I/O failure |

Guards default to `CAPNP_DEFAULT_TRAVERSAL_WORDS` (8 Mi words) and
`CAPNP_DEFAULT_DEPTH_LIMIT` (64); both are per-message and settable through
the deserialize entry points.

Element-size codes for `capnp_new_list`: `CAPNP_SZ_VOID` (0), `_BIT`,
`_BYTE`, `_TWO`, `_FOUR`, `_EIGHT`, `_PTR`, `_COMPOSITE` (7). Handle kinds:
`CAPNP_PK_NULL`, `_STRUCT`, `_LIST`, `_CAP`.

## Messages (`capnp_arena`)

Handles hold a pointer to their message, so message variables must carry
the `target` attribute.

| Procedure | Signature | Notes |
|-----------|-----------|-------|
| `capnp_message_init_builder` | `(msg, err [, first_words])` | fresh builder; reserves the zeroed root word |
| `capnp_message_free` | `(msg)` | releases owned segments; safe on views |

## Objects and pointers (`capnp_message`)

`capnp_ptr_t` is a value-type handle: `kind`, position, and struct/list
geometry. `p%kind == CAPNP_PK_NULL` denotes a null pointer; readers treat
null structs and lists as empty/all-defaults, as the C++ implementation
does.

| Procedure | Signature | Notes |
|-----------|-----------|-------|
| `capnp_root` | `(msg, err) result(p)` | resolve the root pointer |
| `capnp_set_root` | `(msg, p, err)` | write the root pointer |
| `capnp_new_struct` | `(msg, dwords, pwords, err) result(p)` | |
| `capnp_new_list` | `(msg, esize, count, err) result(p)` | primitive/pointer lists |
| `capnp_new_composite_list` | `(msg, count, dwords, pwords, err) result(p)` | writes the tag word |
| `capnp_getp` | `(p, slot, err) result(q)` | read pointer slot; downgrades composite lists when asked for pointers |
| `capnp_setp` | `(p, slot, q, err)` | same-segment relative, cross-segment far, cross-message deep copy |
| `capnp_list_len` | `(l) result(int64)` | |
| `capnp_list_getp` | `(l, i, err) result(q)` | element of a PTR list |
| `capnp_list_get_struct` | `(l, i, err) result(q)` | composite element; upgrades primitive lists to struct views |
| `capnp_copy` | `(dstmsg, src, err) result(q)` | recursive deep copy into another message |
| `capnp_disown` | `(p, i, err) result(q)` | orphan: zero the slot, return the object; re-link with `capnp_setp` |
| `capnp_total_size` | `(p, err) result(int64)` | words reachable from `p` (C++ `totalSize()`); the size a deep copy occupies |

## Field accessors (`capnp_message`)

Scalar accessors take byte offsets into the data section (bit offsets for
bool) and an optional `default` that is XORed on the wire, so generated
code passes declared defaults straight through:

- `capnp_get_i8/i16/i32/i64 (p, byte_off [, default])`,
  `capnp_set_i8/... (p, byte_off, v, err [, default])`
- unsigned views widen the result kind: `u8 -> int16`, `u16 -> int32`,
  `u32 -> int64`; `capnp_get_u64/set_u64` alias the i64 wire ops
- `capnp_get_f32/f64`, `capnp_set_f32/f64` (bit-pattern XOR defaults)
- `capnp_get_bool (p, bit_off [, default])`, `capnp_set_bool`

List element accessors mirror them: `capnp_list_get_<T> (l, i, err)` /
`capnp_list_set_<T> (l, i, v, err)` for i8..i64, f32, f64, plus
`capnp_list_get_bool/set_bool` for bit lists. Whole-list transfers
(`capn_getv`/`capn_setv` parity): `capnp_list_get_all_<T> (l, arr, err)`
allocates and fills `arr`; `capnp_list_set_all_<T>` requires
`size(arr)` equal to the list length. Element reads also work through
schema-evolution views (primitive list read as struct, composite list read
as primitive via field `@0`).

### Text and Data

| Procedure | Signature | Notes |
|-----------|-----------|-------|
| `capnp_get_text` | `(p, slot, str, err)` | null slot yields `''` and `CAPNP_OK` |
| `capnp_set_text` | `(p, slot, str, err)` | writes NUL-terminated byte list |
| `capnp_text_len` | `(p, slot, err) result(int64)` | length without copying, NUL excluded |
| `capnp_list_get_text` / `capnp_list_set_text` | `(l, i, str, err)` | `List(Text)` elements |
| `capnp_get_data` | `(p, slot, b, err)` | allocates and copies |
| `capnp_set_data` | `(p, slot, b, err)` | |
| `capnp_get_data_view` | `(p, slot, view, err)` | zero-copy pointer slice into the message segment |
| `capnp_get_text_view` | `(p, slot, view, err)` | zero-copy character bytes, NUL excluded |

## Unions (`capnp_union`)

| Procedure | Signature | Notes |
|-----------|-----------|-------|
| `capnp_which` | `(p, disc_off16) result(integer)` | discriminant at 16-bit offset `disc_off16` |
| `capnp_set_which` | `(p, disc_off16, w, err)` | generated setters call this before writing the member |

## Serialization (`capnp_serialize`, `capnp_stream`)

| Procedure | Signature | Notes |
|-----------|-----------|-------|
| `capnp_serialize_bytes` | `(msg, bytes, err)` | segment table + segments |
| `capnp_deserialize_bytes` | `(bytes, msg, err [, traversal_words, depth_limit])` | copies segments; frees `msg` first |
| `capnp_deserialize_view` | `(bytes, msg, err [, ...])` | zero-copy: segments alias the caller's `target` buffer |
| `capnp_serialize_packed_bytes` / `capnp_deserialize_packed_bytes` | as above | packed framing |
| `capnp_write_file` / `capnp_read_file` | `(path, bytes, err)` | raw byte I/O |
| `capnp_write_message` / `capnp_read_message` | `(path, msg, err)` | framed message file I/O |
| `capnp_write_message_packed` / `capnp_read_message_packed` | `(path, msg, err)` | |
| `capnp_read_message_unit` | `(unit, msg, err)` | one framed message from an open stream unit; back-to-back messages read in sequence |
| `capnp_read_message_packed_unit` | `(unit, msg, err)` | same for packed streams (C++ `PackedMessageReader`) |

## Packed codec (`capnp_packed`)

| Procedure | Signature | Notes |
|-----------|-----------|-------|
| `capnp_pack` / `capnp_unpack` | `(in, out, err)` | whole buffers |
| `capnp_unpacker_t` + `capnp_unpack_push` | `(u, chunk, out, outn, err)` | incremental; chunks may split anywhere |
| `capnp_packer_t` + `capnp_pack_push` / `capnp_pack_finish` | `(pk, chunk, out, outn, err)` | incremental; byte-identical to `capnp_pack` |

## Canonical form (`capnp_canonical`)

`capnp_canonicalize (msg, bytes, err)` produces the canonical single
segment (no segment table): preorder layout, trailing-zero truncation of
data sections, uniform composite-element trimming. Byte-compatible with
`capnp convert binary:canonical`.

## Wire-level helpers (`capnp_endian`, `capnp_pointer`)

`cp_get_*`/`cp_put_*` compose little-endian scalars from individual bytes
(endianness-independent by construction); `cp_f32_bits`/`cp_bits_f32` and
the f64 pair convert between reals and bit patterns. `wp_*` build and field
raw pointer words; these exist for tests and the code generator, generated
code does not need them.

## C ABI (`capnp_cabi`)

`bind(c)` entry points named `cabi_*` mirror c-capnproto's `capn_*`
surface over pooled integer handles: builder lifecycle, struct/list
allocation (composite and primitive), pointer wiring, scalar and text/data
accessors, union discriminants, serialize/deserialize (flat and packed),
and canonicalization. See `interop/README.md` and the declarations at the
top of `interop/golden_master.c`.

## RPC (`capnp_rpc`, `capnp_posix`, `capnp_rpc_transport`)

Two-party RPC at level 1, single-threaded and message-driven. The
protocol layer (`rpc_capnp`, `rpc_twoparty_capnp`) is generated by
`capnpc-fortran` from the vendored `rpc.capnp` schemas.

| Procedure | Signature | Notes |
|-----------|-----------|-------|
| `rpc_conn_init` | `(conn, fd, bootstrap)` | `bootstrap` is a `class(rpc_server_t), pointer` (may be null) |
| `rpc_conn_close` | `(conn)` | half-closes and frees tables |
| `rpc_bootstrap_send` | `(conn, cap, err)` | returns a pipeline cap, usable immediately |
| `rpc_call_begin` | `(conn, target, interface_id, method_id, m, params, qid, err)` | fill `params` content, then send |
| `rpc_call_send` | `(conn, m, err)` | |
| `rpc_pipeline_cap` | `(qid, field_indices) result(cap)` | promise pipelining into unreturned results |
| `rpc_wait` | `(conn, qid, err)` | pumps until the question returns |
| `rpc_result_content` | `(conn, qid, content, err)` | `RPC_ERR_EXCEPTION` on exception returns |
| `rpc_result_cap` | `(conn, qid, field_indices, cap, err)` | settles a result capability into an import |
| `rpc_finish_send` | `(conn, qid, retain_caps, err)` | |
| `rpc_release_send` | `(conn, cap, err)` | |
| `rpc_pump_once` | `(conn, err)` | handle exactly one incoming message (servers loop on this) |
| `rpc_ctx_export_cap` | `(ctx, srv, err) result(idx)` | stage a capability in a dispatch's results |
| `rpc_make_cap_ptr` | `(m, idx) result(p)` | capability pointer for content slots |
| `rpc_stream_t` + `rpc_stream_init` | `(stream [, window])` | `-> stream` flow control: bounded unacknowledged-call window |
| `rpc_stream_send` | `(conn, stream, m, qid, err)` | send without waiting; blocks only to retire the oldest call when full |
| `rpc_stream_finish` | `(conn, stream, err)` | drain the window; first failure wins, later sends fail fast |

Capability implementations extend `rpc_server_t` and implement
`dispatch(ctx, err)`; `ctx` carries `interface_id`, `method_id`, the
resolved `params` content, and the results payload. Answering
`RPC_PERSISTENT_IFACE` / `RPC_PERSISTENT_SAVE` opts a capability into
level 2 persistence with application-defined SturdyRefs. Level 3/4
messages (provide/accept/join) are answered with `Message.unimplemented`
per the spec, matching capnp-C++. `capnp_posix` provides the socket
surface (socketpair, TCP listen/accept/connect, poll) as pure
`iso_c_binding` interfaces into libc.

## Code generator (`capnpc-fortran`)

Run as a `capnp` plugin: `capnp compile -o <path-to-capnpc-fortran>
file.capnp`, one module per schema file (`<file>_capnp.f90`). Generated
surface per struct `Foo`: `foo_t` handle type, `FOO_DWORDS`/`FOO_PWORDS`,
`foo_new`/`foo_new_root`/`foo_read_root`, per-field
`<field>_get`/`<field>_set` (scalars carry declared defaults),
`<field>_init` for pointer fields, `_get_elem`/`_set_elem` for
`List(Text)`/`List(Data)`, `<union>_which` plus `<PFX>_<MEMBER>_TAG`
constants, group `_select` setters, enum `<TYPE>_<MEMBER>` constants, and
constants (scalar parameters; Data as byte arrays; struct/list consts as
accessor functions over embedded blobs).

Interfaces emit an `INTERFACE_ID` parameter, a `<iface>_client_t` handle,
per-method `_begin`/`_wait` helpers over the vat, and an abstract
`<iface>_server_t` whose generated dispatch routes method ordinals to
deferred typed procedures. `-> stream` methods pair the generated
`_begin` with `rpc_stream_send`/`rpc_stream_finish`.

Branded generic uses (`Box(Text)`) produce brand-resolved
instantiations: a `box_text_t` handle plus the generic's accessors with
type parameters substituted by their bindings (`box_text_value_get`
takes text). Unbound parameters keep AnyPointer accessors; parameters
inside `List(...)` element positions degrade to pointer lists.
