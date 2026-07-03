=======
Interop
=======

.. contents::

This is a how-to for the ``interop`` pixi environment: two
cross-implementation checks that build outside fpm, using Meson, and
compare this runtime's wire output and RPC behavior against reference
Cap'n Proto implementations byte for byte and message for message.

1 Set up the environment
-------------------------

.. code-block:: console

   $ pixi install -e interop

This pulls in Meson, Ninja, cmocka, pkg-config, and C/C++ compilers on
top of the default ``capnproto`` dependency.

2 c-capnproto golden master
-----------------------------

This tier builds the same Cap'n Proto message two ways -- with the
reference C encoder `c-capnproto
<https://github.com/opensourcerouting/c-capnproto>`_ and with this
project's Fortran runtime through the ``capnp_cabi`` ``bind(c)`` shim
(``src/capnp_cabi.f90``) -- and asserts the framed wire bytes are
identical, byte for byte. Further cases decode each encoder's output
with the other decoder, and check the packed encoder against the
spec's worked packing example.

2.1 Fetch the reference sources
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c-capnproto ships an autotools/cmake build this project does not use;
the Meson build compiles its three runtime ``.c`` files directly from
a vendored clone (``third_party/``, untracked):

.. code-block:: console

   $ git clone https://github.com/opensourcerouting/c-capnproto third_party/c-capnproto

The sources must end up under ``third_party/c-capnproto/lib/``
(``capn.c``, ``capn-malloc.c``, ``capn-stream.c``, ``capnp_c.h``,
``capnp_priv.h``). If they are absent, ``meson setup`` still succeeds
but the golden-master executable is skipped with a message.

2.2 Build and run
~~~~~~~~~~~~~~~~~~

.. code-block:: console

   $ pixi run -e interop meson setup build-interop .
   $ pixi run -e interop meson compile -C build-interop
   $ pixi run -e interop meson test -C build-interop

``meson test -C build-interop -v`` prints the per-assertion cmocka
output. The cases:

- ``test_golden_bytes`` -- build the message with both encoders in
  the same allocation order (root struct, then ``name`` text, then
  the composite list) and ``memcmp`` the framed output.
- ``test_cross_decode`` -- decode the c-capnproto bytes with the shim
  getters and the shim's bytes with ``capn_init_mem``, checking every
  field value.
- ``test_packed_vector`` -- run ``capn_deflate`` on the two-word
  packing example from the `encoding spec
  <https://capnproto.org/encoding.html#packing>`_ and assert the
  output is ``51 08 03 02 31 19 aa 01``.
- ``test_packed_golden`` -- pack the golden message with
  ``cabi_serialize_packed`` and with ``capn_deflate`` over the shared
  flat bytes, ``memcmp`` the two, then round-trip the reference's
  packed output through ``cabi_deserialize_packed``.
- ``test_primitive_list_golden`` -- a ``List(Int32)`` built with
  ``capn_new_list32`` / ``capn_set32`` on the reference side and
  ``cabi_new_list`` / ``cabi_list_set_i32`` on this project's side;
  framed bytes must match.
- ``test_canonical_form`` -- ``cabi_canonicalize`` of the golden
  message: 8-word preorder single segment with the composite
  elements' null pointer sections trimmed uniformly, root pointer
  word checked byte for byte.

2.3 The composite-list gate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The message is assembled by hand, with no schema compiler involved:

.. code-block:: capnp

   root :Struct {
     value @0  :UInt32;      # data offset 0
     name  @0p :Text;        # pointer slot 0
     items @1p :List(Elem);  # pointer slot 1, composite, 2 elements
   }
   Elem :Struct {
     n @0  :UInt32;          # one data word, value at offset 0
     _ @0p :AnyPointer;      # spare pointer slot, left null
   }

``Elem`` carries a spare (null) pointer slot on purpose. c-capnproto's
``capn_new_list`` only emits a **composite** list when
``ptrs || datasz > 8``; a one-data-word, zero-pointer struct would be
down-encoded there to a primitive ``List(UInt64)``, whereas this
runtime always emits composite for struct element lists. The spare
slot forces composite encoding on both sides so the golden bytes
match. The ``UInt32`` field at offset 0 is unchanged; the spare slot
stays zero on the wire.

3 capnp-C++ RPC peer
----------------------

A second Meson target protocol-tests the RPC vat (:doc:`rpc`) against
a live capnp-C++ peer, rather than comparing static bytes. It only
configures when a C++ toolchain, ``libcapnp-rpc``, the ``capnp`` tool,
and ``capnpc-c++`` are all found; otherwise Meson prints a skip
message and continues.

- ``interop/rpc_peer_server.c++`` -- a capnp-C++ ``EzRpcServer``
  hosting the ``Adder`` interface (``schema/adder.capnp``), listening
  on a port given as its first argument.
- ``interop/rpc_client.f90`` -- a Fortran client (``capnp_rpc`` +
  ``capnp_posix``) that connects over TCP, bootstraps, and calls
  ``add()`` twice: once pipelined, before the bootstrap call's
  ``Return`` has settled, and once on the settled import. It exits
  nonzero on any mismatch.
- ``interop/run_rpc_interop.sh`` -- orchestrates the pair: starts the
  C++ server on a random high port, runs the Fortran client against
  it, and tears the server down on exit.

Meson registers the pairing as the ``rpc_interop_cpp`` test, so it
runs alongside the golden-master test:

.. code-block:: console

   $ pixi run -e interop meson test -C build-interop rpc_interop_cpp -v

This is the only tier that exercises the vat against an independent
Cap'n Proto RPC implementation rather than against itself, so it is
the check that catches protocol-level mismatches (pipelining,
embargoes, disembargo timing) that a same-process test cannot.
