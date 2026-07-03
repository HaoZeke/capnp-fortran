====================
Library architecture
====================


.. contents::


1 Module layers
---------------

``use capnp`` is the umbrella module for the serialization API. It
re-exports the wire-format and message-I/O layer; RPC and the C ABI
shim are separate modules, used explicitly:

::

    capnp_kinds -> capnp_endian -> capnp_pointer -> capnp_arena -> capnp_message
                                                                        |
                          +---------------------+---------------------+
                          |                      |                     |
                   capnp_serialize        capnp_union          capnp_canonical
                          |
                    capnp_packed
                          |
                    capnp_stream

    capnp (umbrella: everything above)

    capnp_posix -> capnp_rpc_transport -+
    rpc_capnp (generated) --------------+-> capnp_rpc

    capnp_cabi (bind(c) shim over capnp)

``capnp_kinds`` defines the integer/real kind aliases, error codes
(``CAPNP_OK``, ``CAPNP_ERR_*``), and element-size/handle-kind
constants. ``capnp_endian`` composes little-endian scalars byte by
byte, so it is endianness-independent by construction, and converts
between reals and their bit patterns for XORed float defaults.
``capnp_pointer`` is pure bit manipulation on 64-bit pointer words,
with no segment or arena knowledge. ``capnp_arena`` owns segment
storage. ``capnp_message`` combines the three into the reader/builder
object model. Everything downstream of ``capnp_message`` -- packing,
streaming, canonicalization, unions -- builds on that object model,
not on the wire bytes directly.

2 Wire format mapping
---------------------

2.1 Pointer words
~~~~~~~~~~~~~~~~~

Every pointer is one 64-bit little-endian word. ``capnp_pointer``
encodes and decodes the four kinds (bit 0 is the least significant
bit):

::

    struct: kind(0-1)=0 | offset(2-31, signed) | dwords(32-47) | pwords(48-63)
    list:   kind(0-1)=1 | offset(2-31, signed) | esize(32-34)  | count(35-63)
    far:    kind(0-1)=2 | two(2) | pad word offset(3-31) | segment id(32-63)
    cap:    kind(0-1)=3 | zero(2-31) | capability index(32-63)

``capnp_ptr_t`` (in ``capnp_message``) is the value-type handle built
from a resolved pointer word: kind, segment/word position, and
struct/list geometry. A null struct or list pointer
(``p%kind =`` CAPNP\ :sub:`PK`\ \ :sub:`NULL`\=) reads as empty/all-defaults, matching
the C++ implementation. Far and double-far pointers are followed
transparently during resolution (``capnp_getp`` / ``capnp_root``); a
handle returned to the caller always describes the resolved object,
never a far-pointer landing pad.

2.2 Segments and the arena
~~~~~~~~~~~~~~~~~~~~~~~~~~

A message (``capnp_message_t``) owns an array of ``capnp_segment_t``:
a flat ``integer(int8)`` buffer, a used-length prefix, and an
``owned`` flag. Deserializing into a fresh message copies segment
bytes; ``capnp_deserialize_view`` instead aliases the caller's
buffer, so ``owned`` stays ``.false.`` and ``capnp_message_free``
does not deallocate it -- the buffer must outlive the message.

Builder allocation (``capnp_arena_alloc``) is a bump allocator over
the last segment: if the requested words fit in the remaining
capacity, it returns the next byte offset and advances ``len``. If
not, it grows a fresh segment sized at least double the previous
segment's capacity (or the requested size, if larger), matching the
amortized-doubling policy a bump allocator needs to keep allocation
O(1) amortized. ``capnp_arena_alloc_in`` allocates inside a specific
existing segment instead of always the last one -- used only for
far-pointer landing pads, which the spec requires to live in the
segment of the object they describe.

Handles carry a pointer to their owning message (the ``target``
attribute contract documented on every public entry point), because
Fortran has no reference-counted or garbage-collected heap: a
``capnp_ptr_t`` is only as valid as the message whose segments it
indexes into.

2.3 Schema evolution
~~~~~~~~~~~~~~~~~~~~

Reads past the end of a struct's data or pointer section return the
declared default (zero, empty, or the field's default value) rather
than an out-of-bounds error -- a struct written by an older schema
version and read by a newer one silently reports defaults for the
fields it never had. A primitive list read where a struct list is
expected upgrades to a struct view (the primitive occupies field
``@0`` of a one-word data section); a composite list read where a
primitive list is expected downgrades the same way. This matches
capnp-C++'s evolution rules exactly (see the reference's
``capnp_getp`` / ``capnp_list_get_struct`` / ``capnp_list_getp`` entries),
which is what lets messages produced by either implementation
round-trip through the other.

3 Code generator: two-pass emission
-----------------------------------

``capnpc-fortran`` (``app/capnpc_emit.f90``, driven by
``app/capnpc_schema.f90``) is a ``capnp compile -o`` plugin: it reads
a serialized ``CodeGeneratorRequest`` from stdin with hand-rolled
accessors over this project's own runtime (the same bootstrap
approach ``capnpc-c`` uses -- no chicken-and-egg dependency on
generated code to read the generator's own input schema).

Emission for each requested file walks the node graph twice:

- **Pass 1** walks without writing any Fortran source. It collects
  cross-file type references (so ``use`` statements can be emitted
  once, deduplicated, before the first line of module body) and
  serializes every non-null field default into a standalone
  ``capnp`` message, registering it as a named byte blob
  (``<CONST>_DEFAULT``-style parameter arrays).

- **Pass 2** walks the same graph again and writes: the handle type
  per struct, ``<STRUCT>_DWORDS`` / ``<STRUCT>_PWORDS`` size
  constants, ``_new`` / ``_new_root`` / ``_read_root`` constructors,
  per-field accessors (scalar defaults applied via XOR, pointer
  fields getting ``_init`` initializers), union ``_which`` selectors
  and ``_WHICH`` constants, group ``_select`` setters, enum
  constants, and ``interface`` client/server pairs.

Names are snake\ :sub:`cased`\ from capnp's camelCase, with nested scopes
joined by underscores (``Person.PhoneNumber`` becomes
``person_phone_number``). Deeply nested schemas can overflow
Fortran's 63-character identifier limit once accessor suffixes are
appended, so names beyond a threshold compress to a 15-character
head plus the node's unique 16-hex-digit id.

For a file that declares one or more ``interface`` nodes, the
generator adds ``use capnp_rpc`` and, per interface, emits: a client
handle type wrapping an ``rpc_cap_t``, a call-begin/call-wait
procedure pair per method that fills parameters through
``rpc_call_begin`` and blocks on ``rpc_wait``, and an abstract server
base extending ``rpc_server_t`` whose generated ``dispatch``
decodes the interface and method ordinals and routes to a
deferred, per-method procedure the application implements (see
:doc:\`tutorial\`, part 2).

4 RPC vat: message-driven dispatch
----------------------------------

``capnp_rpc`` implements two-party RPC level 1 over a connected
stream socket. The vat is single-threaded and message-driven:
``rpc_pump_once`` decodes exactly one incoming message and updates
connection state accordingly, so a process can run both sides of a
socketpair deterministically (as the test suite does), and a real
server is ``do while (ok); call rpc_pump_once(conn, err); end do``.

Connection state (``rpc_conn_t``) is three fixed-size slot tables
plus liveness flags:

- ``questions(0:63)`` -- calls this vat issued, tracked by id until
  a ``Return`` settles them (``rpc_wait`` pumps until the matching
  slot is marked ``returned``).

- ``answers(0:63)`` -- calls this vat received and is (or already
  did) answer, tracked until the peer sends ``Finish``.

- ``exports(0:63)`` -- capabilities this vat has handed out,
  refcounted; a ``Release`` message decrements and frees at zero.

.. graphviz::

   digraph vat_dispatch {
      fontname="Jost";
      rankdir=TB;
      node [fontname="Jost", fontsize=12, style=filled, fillcolor=white,
            color="#004D40", fontcolor="#004D40"];
      edge [fontname="Jost", fontsize=11, color="#004D40", fontcolor="#004D40"];

      incoming [label="rpc_recv_message"];
      tag [label="message_which(msg)", shape=diamond, fillcolor="#F1DB4B"];
      bootstrap [label="Bootstrap\nexport bootstrap_srv,\nreturn its cap"];
      call [label="Call\ndispatch() on target,\nfill answer slot"];
      ret [label="Return\nsettle question slot,\nunblock rpc_wait"];
      finish [label="Finish\nfree answer slot"];
      release [label="Release\ndecrement export refcount"];
      disembargo [label="Disembargo\necho back (embargo release)"];
      abort [label="Abort", fillcolor="#FF655D", fontcolor=white];
      unimpl [label="Unimplemented /\nlevel 3-4 (Provide,\nAccept, Join)",
              fillcolor="#FF655D", fontcolor=white];

      incoming -> tag;
      tag -> bootstrap;
      tag -> call;
      tag -> ret;
      tag -> finish;
      tag -> release;
      tag -> disembargo;
      tag -> abort;
      tag -> unimpl [label="unrecognized tag"];
   }

A call target can be a settled import (``RPC_CAP_IMPORT``) or a
**pipeline** onto a question that has not returned yet
(``RPC_CAP_PIPELINE``, built by ``rpc_pipeline_cap`` from a question
id plus a chain of pointer-field hops): calls on a pipelined
capability are sent immediately, before the bootstrap or prior call
they depend on has returned, and the peer resolves the hop chain
against its own in-flight answer. ``rpc_result_cap`` later settles a
pipeline reference into a plain import once the dependency returns.

Level 2 persistence is an opt-in hook, not a separate code path: a
capability's ``dispatch`` recognizes
``ctx%interface_id =`` RPC\ :sub:`PERSISTENT`\ \ :sub:`IFACE`\= (capnp's
``Persistent`` interface, id ``0xc8cb212fcd9f5691``) and answers
``save()`` with an application-defined SturdyRef. Level 3 and 4
messages (``Provide``, ``Accept``, ``Join`` -- three-party handoff)
are answered with ``Message.unimplemented``, per the spec, exactly as
capnp-C++ does; no known Cap'n Proto implementation reaches those
levels.

``capnp_rpc_transport`` is the byte layer underneath all of this: it
frames each RPC message with the same segment-table framing as
plain serialization (:doc:\`reference\`, "Serialization") over a file
descriptor from ``capnp_posix``, which wraps just enough POSIX
socket surface (socketpair, TCP listen/accept/connect, poll) as
``iso_c_binding`` interfaces to carry those frames -- no C sources,
interfaces into libc only.
