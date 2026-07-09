=======
Install
=======


.. contents::


1 Requirements
--------------

- A Fortran 2018 compiler (gfortran 13+ is what CI and ``pixi`` pin)

- `fpm <https://fpm.fortran-lang.org>`_ for the library and tests

- `fypp <https://fypp.readthedocs.io>`_ to regenerate ``capnp_endian`` / ``capnp_message`` from ``.fypp`` sources

- The Cap'n Proto ``capnp`` tool when compiling schemas or regenerating fixtures

Optional interop tier:

- meson, ninja, a C/C++ toolchain, cmocka

- libcapnp / libcapnp-rpc for the live C++ RPC peer test

2 fpm
-----

.. code:: console

    $ git clone https://github.com/HaoZeke/capnp-fortran.git
    $ cd capnp-fortran
    $ fpm build
    $ fpm test

The package name in ``fpm.toml`` is ``capnp``; the executable is ``capnpc-fortran``.

3 pixi (recommended)
--------------------

``pixi.toml`` pins gfortran, fpm, fypp, and capnproto on ``linux-64``:

.. code:: console

    $ pixi install
    $ pixi run gen      # fypp → src/*.f90 (commit if templates change)
    $ pixi run build
    $ pixi run test

Environments:

.. table::

    +---------+---------------------------------------------------+
    | Feature | Purpose                                           |
    +=========+===================================================+
    | default | library + tests                                   |
    +---------+---------------------------------------------------+
    | interop | meson + cmocka + C++ for golden master / C++ peer |
    +---------+---------------------------------------------------+
    | docs    | Sphinx + emacs ox-rst export                      |
    +---------+---------------------------------------------------+

4 Interop suite
---------------

.. code:: console

    $ git clone --depth 1 https://github.com/opensourcerouting/c-capnproto third_party/c-capnproto
    $ pixi run -e interop meson setup build-interop .
    $ pixi run -e interop meson compile -C build-interop
    $ pixi run -e interop meson test -C build-interop -v

When libcapnp-rpc is available, meson also builds the Fortran RPC client
against a C++ ``EzRpcServer`` (schema ``adder.capnp``). Details: :doc:`interop`.

5 Building documentation
------------------------

.. code:: console

    $ pixi run -e docs docs
    # HTML under docs/build/

Live site: `https://capnp-fortran.rgoswami.me <https://capnp-fortran.rgoswami.me>`_ (Cloudflare Pages via the Documentation workflow).

6 Using the library from another fpm project
--------------------------------------------

Add a dependency on this repository (or the fpm registry package ``capnp`` once published), then:

.. code:: fortran

    use capnp

Generated schema modules are ordinary Fortran sources you compile alongside your program after running ``capnpc-fortran``.

7 CMake / FetchContent
----------------------

Official Cap'n Proto C++ owns ``find_package(CapnProto)`` and
``CapnProto::capnp`` / ``CapnProto::capnp-rpc``. This Fortran port uses a
**separate** CMake project name and namespace so both can link in one tree:

.. table::

    +------------------+----------------------+----------------------------------+
    | \                | Cap'n Proto C++      | capnp-fortran                    |
    +==================+======================+==================================+
    | ``find_package`` | ``CapnProto``        | ``capnp_fortran``                |
    +------------------+----------------------+----------------------------------+
    | link target      | ``CapnProto::capnp`` | ``capnp_fortran::capnp_fortran`` |
    +------------------+----------------------+----------------------------------+
    | Fortran module   | n/a                  | ``use capnp``                    |
    +------------------+----------------------+----------------------------------+

Top-level ``CMakeLists.txt`` builds the same checked-in ``src/*.f90`` set as the
meson interop tier. Optional plugin: ``CAPNP_FORTRAN_BUILD_PLUGIN`` (default ON
when this tree is the top-level project, OFF under ``FetchContent`` /
``add_subdirectory``). Options use the ``CAPNP_FORTRAN_*`` prefix so they do not
collide with Cap'n Proto's ``CAPNP_*`` variables.

.. code:: cmake

    include(FetchContent)
    FetchContent_Declare(
      capnp_fortran
      GIT_REPOSITORY https://github.com/HaoZeke/capnp-fortran.git
      GIT_TAG        v0.1.1
    )
    # Optional: set(CAPNP_FORTRAN_BUILD_PLUGIN ON CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(capnp_fortran)
    target_link_libraries(myapp PRIVATE capnp_fortran::capnp_fortran)

Standalone:

.. code:: console

    $ cmake -S . -B build-cmake -DCMAKE_BUILD_TYPE=Release
    $ cmake --build build-cmake
    # library: build-cmake/libcapnp_fortran.a  (or .so if CAPNP_FORTRAN_BUILD_SHARED=ON)
    # plugin:  build-cmake/capnpc-fortran   when CAPNP_FORTRAN_BUILD_PLUGIN=ON

Installable package config (``find_package(capnp_fortran)``) is generated when
``CAPNP_FORTRAN_INSTALL`` is ON (default for top-level builds).
