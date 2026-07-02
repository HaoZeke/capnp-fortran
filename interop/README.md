# C-interop golden-master tests

This tier builds the same Cap'n Proto message two ways -- with the reference C
encoder [`opensourcerouting/c-capnproto`][ccapnp] and with this project's
Fortran runtime through the `capnp_cabi` bind(c) shim (`src/capnp_cabi.f90`) --
and asserts the framed wire bytes are identical, byte for byte. A second test
decodes each encoder's output with the other decoder, and a third checks
c-capnproto's packed encoder against the spec worked example.

The build is [Meson][meson] (Fortran + C) and the tests use [cmocka][cmocka].
Everything needed is in the `interop` pixi environment.

## Fetch the reference sources

c-capnproto ships an autotools/cmake build we do not use; the Meson build
compiles its three runtime `.c` files directly from a vendored clone
(`third_party/`, untracked):

```console
$ git clone https://github.com/opensourcerouting/c-capnproto third_party/c-capnproto
```

The sources must end up under `third_party/c-capnproto/lib/` (`capn.c`,
`capn-malloc.c`, `capn-stream.c`, `capnp_c.h`, `capnp_priv.h`). If they are
absent, `meson setup` still succeeds but the golden-master executable is
skipped with a message.

## Build and run

```console
$ pixi run -e interop meson setup build-interop .
$ pixi run -e interop meson compile -C build-interop
$ pixi run -e interop meson test -C build-interop
```

`meson test -C build-interop -v` prints the per-assertion cmocka output.

## What the tests cover

- **`test_golden_bytes`** -- build the message with both encoders in the same
  allocation order (root struct, then `name` text, then the composite list) and
  `memcmp` the framed output.
- **`test_cross_decode`** -- decode the c-capnproto bytes with the shim getters
  and the shim's bytes with `capn_init_mem`, checking every field value.
- **`test_packed_vector`** -- run `capn_deflate` on the two-word packing example
  from the [encoding spec][packing] and assert the output is
  `51 08 03 02 31 19 aa 01`.

## Schema and the composite-list gate

The message is assembled by hand (no schema compiler):

```capnp
root :Struct {
  value @0  :UInt32;      # data offset 0
  name  @0p :Text;        # pointer slot 0
  items @1p :List(Elem);  # pointer slot 1, composite, 2 elements
}
Elem :Struct {
  n @0  :UInt32;          # one data word, value at offset 0
  _ @0p :AnyPointer;      # spare pointer slot, left null
}
```

`Elem` carries a spare (null) pointer slot on purpose. c-capnproto's
`capn_new_list` only emits a **composite** list when `ptrs || datasz > 8`; a
one-data-word, zero-pointer struct would be down-encoded there to a primitive
`List(UInt64)`, whereas this runtime always emits composite. The spare slot
forces composite on both sides so the golden bytes match. The `UInt32` field at
offset 0 is unchanged; the spare slot stays zero on the wire.

[ccapnp]: https://github.com/opensourcerouting/c-capnproto
[meson]: https://mesonbuild.com/
[cmocka]: https://cmocka.org/
[packing]: https://capnproto.org/encoding.html#packing
