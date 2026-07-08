========
Tutorial
========


.. contents::

This page has two parts: writing and reading a plain message, then
running a typed RPC client and server generated from an interface
schema. Both build on the same runtime and the same generated-code
conventions, so the second part only adds ``capnp_rpc`` and a socket.

1 Write and read a message
--------------------------

Toolchain (gfortran, fpm, fypp, the ``capnp`` tool) is pinned in
``pixi.toml``:

.. code:: console

    $ pixi install
    $ pixi run build
    $ pixi run test

Compile the tutorial schema with the ``capnp`` tool, pointing it at the
``capnpc-fortran`` plugin built by fpm:

.. code:: console

    $ capnp compile -o build/gfortran_*/app/capnpc-fortran schema/addressbook.capnp

This writes ``addressbook_capnp.f90`` next to the schema, one Fortran
module for the file. ``schema/addressbook.capnp`` declares:

.. code:: capnp

    struct Person {
      id @0 :UInt32;
      name @1 :Text;
      email @2 :Text;
      phones @3 :List(PhoneNumber);
      # ...
    }

    struct AddressBook {
      people @0 :List(Person);
    }

Build a message, serialize it, then deserialize the bytes and read a
field back:

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

Two conventions to carry into every program that touches this API:

- **Messages carry** ``target``. Handles (``capnp_ptr_t``, and every
  generated ``<type>_t``) hold a pointer to their owning message, so
  ``msg`` and ``rmsg`` above must both be declared ``target``.

- **Every fallible call returns** ``err``. ``CAPNP_OK`` (0) means
  success; readers never crash on malformed input, they return an
  error code and defaults instead. Production code checks ``err``
  after each call; this listing omits the checks for brevity.

2 A typed RPC client and server
-------------------------------

The code generator also emits client stubs and an abstract server base
for ``interface`` declarations. This section builds both ends of a
call over a single process, connected by a socket pair, using the
``Adder`` interface from ``schema/adder.capnp``:

.. code:: capnp

    interface Adder @0xea01e10cbc414411 {
      add @0 (a :Int64, b :Int64) -> (sum :Int64);
    }

Compile it the same way as the struct schema above; the generator
adds an ``adder_capnp`` module with:

- ``adder_client_t`` -- a client handle wrapping an ``rpc_cap_t``.

- ``adder_server_t`` -- an abstract, extensible server base with one
  deferred procedure, ``add(self, params, results, err)``.

- ``adder_add_begin`` / ``adder_add_wait`` -- fill parameters and
  send a call, then block until the result returns.

2.1 Implement the server
~~~~~~~~~~~~~~~~~~~~~~~~

Extend ``adder_server_t`` and implement ``add``:

.. code:: fortran

    module rpc_adder_impl
       use capnp
       use adder_capnp
       implicit none
       private

       public :: my_adder_t

       type, extends(adder_server_t) :: my_adder_t
       contains
          procedure :: add => my_add
       end type my_adder_t

    contains

       subroutine my_add(self, params, results, err)
          class(my_adder_t), intent(inout) :: self
          type(adder_add_params_t), intent(in) :: params
          type(adder_add_results_t), intent(in) :: results
          integer, intent(out) :: err
          err = CAPNP_OK
          call adder_add_results_sum_set(results, &
                                         adder_add_params_a_get(params) + &
                                         adder_add_params_b_get(params), err)
       end subroutine my_add

    end module rpc_adder_impl

The generated ``dispatch`` procedure on ``adder_server_t`` decodes the
interface and method ordinals from an incoming call and routes to
``add`` -- application code never touches ordinals directly.

2.2 Wire up a connection and call it
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: fortran

    program adder_demo
       use capnp
       use adder_capnp
       use capnp_posix
       use capnp_rpc
       use rpc_adder_impl
       implicit none

       type(rpc_conn_t), target :: cli, srv
       type(my_adder_t), target :: impl
       class(rpc_server_t), pointer :: boot
       type(adder_client_t) :: client
       type(capnp_message_t), target :: m
       type(adder_add_params_t) :: params
       type(adder_add_results_t) :: results
       integer(int64) :: qid
       integer :: fda, fdb, err

       ! A socketpair stands in for a TCP connection; rpc_conn_init takes
       ! any connected stream file descriptor.
       call px_socketpair(fda, fdb, err)
       boot => impl
       call rpc_conn_init(srv, fdb, boot)
       boot => null()
       call rpc_conn_init(cli, fda, boot)

       ! Bootstrap resolves the typed client; the pipeline cap works
       ! before the bootstrap call even returns.
       call rpc_bootstrap_send(cli, client%cap, err)
       call rpc_pump_once(srv, err)

       call adder_add_begin(cli, client, m, params, qid, err)
       call adder_add_params_a_set(params, 19_int64, err)
       call adder_add_params_b_set(params, 23_int64, err)
       call rpc_call_send(cli, m, err)
       call rpc_pump_once(srv, err)

       call adder_add_wait(cli, qid, results, err)
       print '(a,i0)', 'sum = ', adder_add_results_sum_get(results)  ! 42

       call rpc_conn_close(cli)
       call rpc_conn_close(srv)
    end program adder_demo

Both ends run in this one process for the tutorial; ``capnp_posix``
also exposes ``px_tcp_listen`` / ``px_tcp_accept`` / ``px_tcp_connect``
for a real client and server on separate hosts, and
``rpc_pump_once`` is meant to sit inside a server's event loop
(``do while (ok); call rpc_pump_once(conn, err); end do``) rather than
being called once per exchange as above. The full working version,
including a second call issued against the settled import capability,

is ``test/test_rpc_typed.f90``; :doc:`rpc` covers the call lifecycle
(bootstrap, pipelining, finish/release) in more detail.
