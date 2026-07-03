
=================
``capnp-fortran``
=================

Native modern-Fortran (F2018) implementation of `Cap'n Proto
<https://capnproto.org>`_ serialization: the wire format runtime, stream
framing, the packed codec, canonicalization, and a ``capnpc-fortran``
schema compiler backend. No C library underneath; only
``iso_fortran_env`` kinds and, for the optional C API, ``iso_c_binding``.

.. important::

   **New here?** -> :doc:`tutorial`

   **Wire format, arena, code generator, RPC vat internals?** -> :doc:`architecture`

   **Full procedure list?** -> :doc:`reference`

   **c-capnproto golden master, capnp-C++ RPC peer?** -> :doc:`interop`

Quick example
-------------

Compile a schema, then build and read back a message:

.. code:: console

    $ capnp compile -o build/gfortran_*/app/capnpc-fortran schema/addressbook.capnp

.. code:: fortran

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

Messages carry ``target`` because handles hold a pointer to their message.
Every fallible call returns an ``err`` code (``CAPNP_OK`` on success);
readers never crash on malformed input, they return errors and defaults.

.. grid:: 1 1 2 2
   :gutter: 2

   .. grid-item-card:: Tutorial
      :link: tutorial
      :link-type: doc

      Write and read a message, then run a typed RPC client/server.

   .. grid-item-card:: Architecture
      :link: architecture
      :link-type: doc

      Wire format mapping, the growable segment arena, the two-pass
      emitter, and the RPC vat state machine.

   .. grid-item-card:: Interop
      :link: interop
      :link-type: doc

      c-capnproto golden-master byte comparison and a live capnp-C++
      RPC peer.

   .. grid-item-card:: RPC
      :link: rpc
      :link-type: doc

      Two-party level 1 RPC: bootstrap, calls, promise pipelining,
      level 2 persistence hooks.

Parity
------

Feature coverage against the two reference serialization implementations,
capnp-c (`c-capnproto <https://github.com/opensourcerouting/c-capnproto>`_)
and capnp-C++:

.. table::

    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | Feature                                                           | capnp-c | capnp-C++                      | capnp-fortran                                             |
    +===================================================================+=========+================================+===========================================================+
    | Wire format read/write (all pointer kinds)                        | yes     | yes                            | yes                                                       |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | Stream framing                                                    | yes     | yes                            | yes                                                       |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | Packed codec                                                      | yes     | yes                            | yes, plus incremental pack/unpack                         |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | Zero-copy reads from a caller buffer                              | yes     | yes                            | yes (``capnp_deserialize_view``, ``capnp_get_data_view``) |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | Traversal and depth limits                                        | no      | yes                            | yes                                                       |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | Schema-evolution reads (defaults past end, list up/downgrades)    | partial | yes                            | yes                                                       |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | Deep copy / cross-message set                                     | no      | yes                            | yes                                                       |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | Orphans (disown/adopt)                                            | no      | yes                            | yes                                                       |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | Canonical form                                                    | no      | yes                            | yes (byte-parity tested)                                  |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | Code generator plugin (``capnp compile -o``)                      | yes     | yes                            | yes (``capnpc-fortran``)                                  |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | RPC level 1 (calls, cap tables, promise pipelining, embargo echo) | no      | yes                            | yes (``capnp_rpc``, two-party)                            |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | RPC level 2 (persistence hooks)                                   | no      | partial                        | hooks (``RPC_PERSISTENT_IFACE``, app-defined SturdyRefs)  |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | RPC level 3/4 (three-party, joins)                                | no      | no (replies ``unimplemented``) | no (replies ``unimplemented``, same as C++)               |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | ``-> stream`` flow control                                        | no      | yes                            | yes (``rpc_stream_t``, windowed)                          |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | Typed interface stubs in generated code                           | no      | yes                            | yes (client helpers + abstract server base)               |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | Generics in generated code                                        | no      | yes                            | brand-resolved instantiations (direct bindings)           |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+
    | Dynamic reflection API                                            | no      | yes                            | yes (``capnp_dynamic``, by-name read/write)               |
    +-------------------------------------------------------------------+---------+--------------------------------+-----------------------------------------------------------+

The full table, including generated-code coverage (unions, groups,
defaults, imports, constants), lives in the project
:footcite:\`goswami2026capnpfortran\` README.

Site map
--------

.. toctree::
   :maxdepth: 1
   :caption: Tutorial

   tutorial

.. toctree::
   :maxdepth: 1
   :caption: How-to

   interop
   rpc

.. toctree::
   :maxdepth: 1
   :caption: Explanation

   architecture

.. toctree::
   :maxdepth: 1
   :caption: Reference

   reference

.. footbibliography::
