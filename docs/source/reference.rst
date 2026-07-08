=============
API reference
=============


.. contents::

This page documents the **public** surface an application programmer calls.
Private helpers inside a module are omitted. Fallible operations return
an integer ``err``; ``CAPNP_OK`` (0) is success. Byte buffers are
``integer(int8)`` arrays (0-based). Word offsets and lengths use
``integer(int64)``.

1 Modules at a glance
---------------------

.. list-table::
   :header-rows: 1
   :widths: 45 55

   * - Module
     - Role
   * - ``capnp``
     - Umbrella: re-exports serialization API
   * - ``capnp_kinds``
     - Kinds, error codes, size/kind constants
   * - ``capnp_message`` / ``capnp_arena``
     - Messages, pointers, field accessors
   * - ``capnp_union``
     - Discriminants
   * - ``capnp_serialize`` / ``capnp_stream``
     - Framed I/O
   * - ``capnp_packed``
     - Packed codec
   * - ``capnp_canonical``
     - Canonical single-segment form
   * - ``capnp_cabi``
     - ``bind(c)`` shim (separate ``use``)
   * - ``capnp_rpc`` + ``capnp_posix`` + ``capnp_rpc_transport``
     - Two-party RPC (separate ``use``)
   * - ``capnp_dynamic`` / ``capnp_schema``
     - Dynamic reflection over CGR
   * - Generated ``<file>_capnp``
     - Schema-specific handles (see :doc:`codegen`)

2 Conventions
-------------

1. Declare messages with the ``target`` attribute; handles store a pointer to the owning message.

2. Check ``err`` after every fallible call in production code.

3. Readers return errors and defaults on malformed input — they do not abort.

3 Full procedure tables
-----------------------

The complete tables (errors, messages, pointers, fields, text/data, unions,
serialize, packed, canonical, C ABI, RPC, codegen naming) are maintained as
Markdown for dense tables and included here:

.. include:: ../reference.md
   :parser: myst_parser.sphinx_

4 Related pages
---------------

- :doc:`tutorial` — first message and typed RPC
- :doc:`codegen` — plugin usage and generated shapes
- :doc:`architecture` — layers and wire mapping
- :doc:`rpc` — vat behaviour and levels
