===
RPC
===


.. contents::

Two-party Cap'n Proto RPC, level 1: bootstrap, calls on imported and
promised (pipelined) capabilities, returns with capability tables,
finish/release bookkeeping, disembargo echo, abort, and the
spec-mandated ``unimplemented`` reply for level 3+ messages. The vat
is single-threaded and message-driven -- see :doc:`architecture`
section 4 for how ``rpc_pump_once`` dispatches one message at a time
and what state it updates.

1 Call sequence
---------------

.. mermaid::

   sequenceDiagram
      participant C as Client vat
      participant S as Server vat
      C->>S: Bootstrap
      Note over C: rpc_bootstrap_send returns a pipeline cap<br/>immediately, before any reply arrives
      S->>S: rpc_pump_once: export bootstrap_srv
      S-->>C: Return (capability descriptor)
      C->>S: Call add(19, 23) on the pipeline cap
      Note over C: sent right after bootstrap_send;<br/>does not wait for the Bootstrap Return
      S->>S: rpc_pump_once: dispatch routes to adder_server_t%add
      S-->>C: Return (sum = 42)
      C->>C: rpc_wait / rpc_result_content settle the answer
      Note over C: rpc_result_cap later turns the pipeline<br/>reference into a settled RPC_CAP_IMPORT

A *pipeline* capability (``RPC_CAP_PIPELINE``) references a
question that has not returned yet, plus a chain of pointer-field
hops into its eventual results (``rpc_pipeline_cap``). Calls on it go
out immediately; the peer resolves the hop chain against its own
in-flight answer when the dependency completes. Generated client code
(:doc:`tutorial`, part 2) always returns a pipeline-capable cap from
a bootstrap or a call that yields a capability result, so pipelining
needs no separate opt-in.

2 Writing a server
------------------

Extend ``rpc_server_t`` and implement ``dispatch``, or -- for
schema-generated interfaces -- extend the generated
``<interface>_server_t`` base and implement its one deferred
per-method procedure (:doc:`tutorial`, part 2.1). A capability object
stays alive as long as it has a nonzero refcount in some vat's export
table; ``rpc_ctx_export_cap`` stages a freshly minted capability
(e.g. one method's result is itself a capability) into the current
call's results.

To opt a capability into level 2 persistence, recognize
``ctx%interface_id == RPC_PERSISTENT_IFACE`` in ``dispatch`` and
answer ``RPC_PERSISTENT_SAVE`` with an application-defined SturdyRef
(any content the application chooses to write into the results
struct). Level 3/4 messages (``Provide``, ``Accept``, ``Join``) need
no application code; the vat answers them with
``Message.unimplemented`` on its own, matching capnp-C++.

3 Writing a client
------------------

.. code:: fortran

    call rpc_bootstrap_send(conn, cap, err)     ! pipeline cap, usable now
    call rpc_call_begin(conn, target, iface_id, method_id, m, params, qid, err)
    ! ... fill params content ...
    call rpc_call_send(conn, m, err)
    call rpc_wait(conn, qid, err)                ! pumps until qid returns
    call rpc_result_content(conn, qid, content, err)

Schema-generated client procedures (``<method>_begin`` /
``<method>_wait``) wrap exactly this sequence with typed parameter
and result handles; use them instead of the untyped calls above
whenever a schema is available (:doc:`tutorial`, part 2.2).

4 Transport
-----------

``capnp_rpc_transport`` frames each RPC message with the same
segment-table framing plain serialization uses
(:doc:`reference`, "Serialization"), over a file descriptor from
``capnp_posix``. ``capnp_posix`` is a thin ``bind(c)`` surface over
POSIX sockets -- no C sources, only ``iso_c_binding`` interfaces into
libc -- covering stream sockets (TCP and Unix-domain), socketpair
loopbacks for tests, and poll-based readiness.

5 Procedure reference
---------------------

.. table::

    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | Procedure              | Signature                                                        | Notes                                                             |
    +========================+==================================================================+===================================================================+
    | ``rpc_conn_init``      | ``(conn, fd, bootstrap)``                                        | ``bootstrap`` is a ``class(rpc_server_t), pointer`` (may be null) |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | ``rpc_conn_close``     | ``(conn)``                                                       | half-closes and frees tables                                      |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | ``rpc_bootstrap_send`` | ``(conn, cap, err)``                                             | returns a pipeline cap, usable immediately                        |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | ``rpc_call_begin``     | ``(conn, target, interface_id, method_id, m, params, qid, err)`` | fill ``params`` content, then send                                |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | ``rpc_call_send``      | ``(conn, m, err)``                                               | \                                                                 |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | ``rpc_pipeline_cap``   | ``(qid, field_indices) result(cap)``                             | promise pipelining into unreturned results                        |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | ``rpc_wait``           | ``(conn, qid, err)``                                             | pumps until the question returns                                  |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | ``rpc_result_content`` | ``(conn, qid, content, err)``                                    | ``RPC_ERR_EXCEPTION`` on exception returns                        |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | ``rpc_result_cap``     | ``(conn, qid, field_indices, cap, err)``                         | settles a result capability into an import                        |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | ``rpc_finish_send``    | ``(conn, qid, retain_caps, err)``                                | \                                                                 |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | ``rpc_release_send``   | ``(conn, cap, err)``                                             | \                                                                 |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | ``rpc_pump_once``      | ``(conn, err)``                                                  | handle exactly one incoming message (servers loop on this)        |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | ``rpc_ctx_export_cap`` | ``(ctx, srv, err) result(idx)``                                  | stage a capability in a dispatch's results                        |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+
    | ``rpc_make_cap_ptr``   | ``(m, idx) result(p)``                                           | capability pointer for content slots                              |
    +------------------------+------------------------------------------------------------------+-------------------------------------------------------------------+

The full public surface, including the umbrella serialization API and
the C ABI shim, is in :doc:`reference`.
