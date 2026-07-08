
.. raw:: html

   <div class="cf-hero">
     <div class="cf-hero-brand">
       <img class="cf-hero-mark" src="_static/mark.svg" width="56" height="56" alt="" />
       <div>
         <p class="cf-hero-name">capnp-fortran</p>
       </div>
     </div>
     <p class="cf-hero-tagline">Native modern-Fortran Cap&rsquo;n Proto: wire runtime, packed and canonical codecs, <code>capnpc-fortran</code> schema plugin, optional C ABI, and two-party RPC. No C library underneath.</p>
     <div class="cf-hero-pills">
       <span>F2018</span>
       <span>wire format</span>
       <span>capnpc plugin</span>
       <span>RPC L1</span>
       <span>fpm</span>
     </div>
     <p class="cf-hero-links">
       <a href="install.html">Install</a>
       <a href="tutorial.html">First message</a>
       <a href="https://github.com/HaoZeke/capnp-fortran">GitHub</a>
     </p>
   </div>

New here?
---------

.. table::

    +---------------------------------------------------+-------------------------------------------------------------+
    | Goal                                              | Go to                                                       |
    +===================================================+=============================================================+
    | Install fpm / pixi and build the suite            | `Install <install.rst>`_                                    |
    +---------------------------------------------------+-------------------------------------------------------------+
    | Write and read a message from a ``.capnp`` schema | `Tutorial <tutorial.rst>`_                                  |
    +---------------------------------------------------+-------------------------------------------------------------+
    | Generate modules with ``capnpc-fortran``          | `Code generation <codegen.rst>`_                            |
    +---------------------------------------------------+-------------------------------------------------------------+
    | Call a capability over a socket                   | `Tutorial (RPC section) <tutorial.rst>`_ + `RPC <rpc.rst>`_ |
    +---------------------------------------------------+-------------------------------------------------------------+
    | Byte-compatible with c-capnproto / C++            | `Interop <interop.rst>`_                                    |
    +---------------------------------------------------+-------------------------------------------------------------+
    | Full procedure list                               | `API reference <reference.rst>`_                            |
    +---------------------------------------------------+-------------------------------------------------------------+

Install (shortest path)
-----------------------

With `fpm <https://fpm.fortran-lang.org>`_ and a recent gfortran:

.. code:: console

    $ git clone https://github.com/HaoZeke/capnp-fortran.git
    $ cd capnp-fortran
    $ fpm build && fpm test

Pinned toolchain (gfortran, fpm, fypp, ``capnp``):

.. code:: console

    $ pixi install
    $ pixi run build && pixi run test

The plugin binary lands under ``build/gfortran_*/app/capnpc-fortran``. Details, codegen, and interop builds: `Install <install.rst>`_.

What you get
------------

- **Wire runtime**: struct / list / far / double-far / capability pointers, growable arena, default-XOR scalars, Text/Data, traversal and depth guards

- **Serialization**: stream framing, packed (whole-buffer + incremental), file helpers, zero-copy views, orphans, deep copy

- **Canonical form**: byte-parity with ``capnp convert binary:canonical``

- **Codegen**: ``capnpc-fortran`` as a ``capnp compile -o`` plugin — structs, unions, groups, enums, constants, imports, branded generics, typed interface stubs

- **C ABI**: ``capnp_cabi`` + cmocka golden master vs `c-capnproto <https://github.com/opensourcerouting/c-capnproto>`_

- **RPC**: two-party level 1 vat (bootstrap, calls, pipelining, embargo), level 2 persistence hooks, optional live C++ peer test

Parity table and honesty about L3/L4 (unimplemented, same as C++): see the project README.

Documentation map
-----------------

.. grid:: 1 2 2 2
   :gutter: 2

   .. grid-item-card:: Install
      :link: install
      :link-type: doc

      fpm, pixi, building the plugin, and optional interop deps.

   .. grid-item-card:: Tutorial
      :link: tutorial
      :link-type: doc

      First message round-trip, then a typed Adder RPC client and server.

   .. grid-item-card:: Code generation
      :link: codegen
      :link-type: doc

      ``capnp compile -o capnpc-fortran`` and the shape of generated modules.

   .. grid-item-card:: Architecture
      :link: architecture
      :link-type: doc

      Module layers, wire mapping, arena, emitter, and the RPC vat.

   .. grid-item-card:: Interop
      :link: interop
      :link-type: doc

      c-capnproto golden master and the capnp-C++ EzRpc peer.

   .. grid-item-card:: RPC
      :link: rpc
      :link-type: doc

      Two-party level 1: tables, pipeline, streams, persistence hooks.

   .. grid-item-card:: API reference
      :link: reference
      :link-type: doc

      Public procedures for messages, fields, pack, canonical, C ABI, RPC.

.. toctree::
   :maxdepth: 1
   :caption: Tutorials
   :hidden:

   tutorial

.. toctree::
   :maxdepth: 1
   :caption: How-to
   :hidden:

   install
   codegen
   interop
   rpc

.. toctree::
   :maxdepth: 1
   :caption: Explanation
   :hidden:

   architecture

.. toctree::
   :maxdepth: 1
   :caption: Reference
   :hidden:

   reference

.. footbibliography::
